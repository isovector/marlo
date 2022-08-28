{-

CREATE SEQUENCE doc_id_seq;
CREATE TABLE IF NOT EXISTS discovery (
  doc_id int8 PRIMARY KEY,
  domain int8 REFERENCES domains(id),
  uri TEXT UNIQUE NOT NULL,
  state VARCHAR(16) NOT NULL,
  depth int4 NOT NULL,
  distance int2[] NOT NULL,
  data bytea NOT NULL,
  headers text[] NOT NULL,
  title TEXT NOT NULL,
  headings TEXT NOT NULL,
  content TEXT NOT NULL,
  comments TEXT NOT NULL,

  js int4 NOT NULL,
  css int4 NOT NULL,
  tweets int2 NOT NULL,
  gifs int2 NOT NULL,
  cookies bool NOT NULL
);

CREATE INDEX depth_idx ON discovery (depth);

CREATE INDEX doc_length_idx ON discovery (length(content));

ALTER TABLE discovery
    ADD COLUMN search tsvector
    GENERATED ALWAYS AS (setweight(to_tsvector('english', title), 'A')
               || ' ' || setweight(to_tsvector('english', headings), 'B')
               || ' ' || setweight(to_tsvector('english', content), 'C')
               || ' ' || setweight(to_tsvector('english', comments), 'D')
                ) STORED;

CREATE INDEX search_idx ON discovery USING GIN (search);

-}

module DB.Document where

import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Int (Int32, Int16)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Rel8.TextSearch
import Types
import DB.PageRawData
import DB.PageContent
import DB.PageStats
import DB.RootSites


data Document f = Document
  { d_docId :: Column f DocId
  , d_domain :: Column f (Maybe DomainId)
  , d_title    :: Column f Text
  , d_uri   :: Column f Text

  -- discovery
  , d_state :: Column f DocumentState
  , d_depth :: Column f Int32

  , d_distance :: Column f [Maybe Int16]

  , d_raw   :: PageRawData f
  , d_page  :: PageContent f
  , d_stats :: PageStats f
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (Document Identity)


documentSchema :: TableSchema (Document Name)
documentSchema = TableSchema
  { name    = "discovery"
  , schema  = Just "public"
  , columns = Document
      { d_docId = "doc_id"
      , d_domain = "domain"
      , d_uri   = "uri"
      , d_title = "title"
      , d_state = "state"
      , d_depth = "depth"
      , d_distance = "distance"
      , d_raw = PageRawData
          { prd_data  = "data"
          , prd_headers  = "headers"
          }
      , d_page = PageContent
          { pc_headings = "headings"
          , pc_content = "content"
          , pc_comments = "comments"
          }
      , d_stats = PageStats
          { ps_js = "js"
          , ps_css = "css"
          , ps_tweets = "tweets"
          , ps_gifs = "gifs"
          , ps_cookies = "cookies"
          }
      }
  }


nextDocId :: Query (Expr DocId)
nextDocId = fmap coerce $ pure $ nextval "doc_id_seq"


data Document' f = Document'
  { d_table  :: Document f
  , d_search :: Column f Tsvector
  }
  deriving stock Generic
  deriving anyclass Rel8able


documentSchema' :: TableSchema (Document' Name)
documentSchema' = documentSchema
  { columns = Document'
      { d_table = columns documentSchema
      , d_search = "search"
      }
  }


emptyDoc :: Document Identity
emptyDoc = Document
  { d_docId = DocId 0
  , d_uri   = ""
  , d_domain = Nothing
  , d_title = ""
  , d_state = Discovered
  , d_depth = 0
  , d_raw = PageRawData
      { prd_data = ""
      , prd_headers = []
      }
  , d_distance = replicate numRootSites Nothing
  , d_page = PageContent
      { pc_headings = ""
      , pc_content  = ""
      , pc_comments = ""
      }
  , d_stats = PageStats
      { ps_js      = 0
      , ps_css     = 0
      , ps_tweets  = 0
      , ps_gifs    = 0
      , ps_cookies = False
      }
  }

