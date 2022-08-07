# marlo

## Dedication

> He who would search for pearls must dive below.
>
> --John Dryden


## Overview

Google is getting worse. I wrote a [bit of a
manifesto](https://reasonablypolymorphic.com/blog/monotonous-web/index.html)
describing the problems. `marlo` is my attempt at a solution.

Doing good *search* on the internet is going to require a massive corpus, which
isn't feasible with my resources. So instead, let's change the equation.

`marlo` isn't a search engine. It's a **discovery engine**.

Search engines are good at giving you answers to facts. For all other queries,
you don't actually know what an answer would look like. Thus, you want to be
easily able to get a lay of the land and canvas opinions to help form yours.


## Concept - Joins

As a multifacted human, you have a wide variety of interests. You probably know
of a few good websites for each interest, which should be marked as your *root
sites.*

The `marlo` spider crawls the web, starting at your root nodes, and expanding
its understanding of the internet outwards. It will download each page, extract
all links from it, and then go download those pages too, ad infinitum.

It's important that your root nodes be diverse; don't pick websites that link to
one another. The spider will follow all links, so it'll get there soon enough.

What's interesting is what happens when the root nodes find one another --- that
is, when they both transitively link to a particular page. The first page
reachable by two or more root nodes is known as a *join*, and corresponds to a
part of the web that lies at the intersection of your interests.

These join pages are where you can find the people you've been looking for.

Mathematically, we're looking at joins on the distance-from-root-nodes poset
we're building.


## Getting Set Up

Install [stack](https://docs.haskellstack.org/en/stable/README/). You can then
build `marlo` via:

```bash
$ git clone https://github.com/isovector/marlo.git
$ cd marlo
$ stack build
```

The whole system is in the resulting `marlo` binary, which you can install
globally via

```bash
$ stack install
```

or run locally via

```bash
$ stack run marlo -- --help
```

Of course, you don't need to build it for yourself, because it's running
[online.](http://marlo.sandymaguire.me)

Oh yeah, and there is a bunch of database stuff you'll have to do that I haven't
automated or documented anywhere. Do a `grep` for `CREATE TABLE` to find
the necessary remnants.

Don't forget to change `config` to point at your local postgres database.


## Commands

```haskell
$ stack run marlo spider
```

Start the spider. Because I'm bad at programming, it will continue running until
you hit `^C` twice in rapid succession.

```haskell
# stack run marlo search
```

Run the main search engine server!


```haskell
$ stack run marlo root-distance
```

Recompute the root-distance metric, which can sometimes get thrown off by the
spider when it discovers a backlink between descendants of different root nodes.

