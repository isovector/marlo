{-


CREATE SEQUENCE doc_id_seq;
CREATE TABLE IF NOT EXISTS documents (
  doc_id int8 PRIMARY KEY,
  domain int8 REFERENCES domains(id),
  uri TEXT UNIQUE NOT NULL,
  state VARCHAR(16) NOT NULL,
  distance int2[] NOT NULL,
  title TEXT NOT NULL,
  search tsvector NOT NULL,

  js int4 NOT NULL,
  css int4 NOT NULL,
  tweets int2 NOT NULL,
  gifs int2 NOT NULL,
  cookies bool NOT NULL
);

CREATE INDEX depth_idx ON documents (depth);

CREATE INDEX doc_length_idx ON documents (length(content));

CREATE INDEX search_idx ON documents USING GIN (search);

-}

module DB.Document where

import DB.PageStats
import DB.RootSites
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Int (Int16)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Rel8 hiding (Enum)
import Rel8.TextSearch
import Types

data Document f = Document
  { d_docId    :: Column f DocId
  , d_domain   :: Column f (Maybe DomainId)
  , d_uri      :: Column f Text

  , d_title    :: Column f Text
  , d_search   :: ~(Column f Tsvector)

  , d_state    :: Column f DocumentState

  , d_distance :: Column f [Maybe Int16]
  , d_stats    :: PageStats f
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving instance Show (Document Identity)


documentSchema :: TableSchema (Document Name)
documentSchema = TableSchema
  { name    = "documents"
  , schema  = Just "public"
  , columns = Document
      { d_docId = "doc_id"
      , d_domain = "domain"
      , d_uri   = "uri"
      , d_title = "title"
      , d_search = "search"
      , d_state = "state"
      , d_distance = "distance"
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


emptyDoc :: Document Identity
emptyDoc = Document
  { d_docId = DocId 0
  , d_uri   = ""
  , d_domain = Nothing
  , d_title = ""
  , d_state = Discovered
  , d_search = Tsvector []
  , d_distance = replicate numRootSites Nothing
  , d_stats = PageStats
      { ps_js      = 0
      , ps_css     = 0
      , ps_tweets  = 0
      , ps_gifs    = 0
      , ps_cookies = False
      }
  }

