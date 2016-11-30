# Reklama #

[![Build Status](https://travis-ci.com/Leonidas-from-XIV/reklama.svg?token=ikxzhRzSB2xgcAYcBv1R&branch=master)](https://travis-ci.com/Leonidas-from-XIV/reklama)

Reklama is a proof of concept for serving ads.

## Building ##

As a native program, the build story can be a bit tough since you need to
install dependencies.

### Locally ###

You need a system with [OPAM](https://opam.ocaml.org/) and a recent-ish OCaml
compiler, so something 4.02 or newer. It was developed on OCaml 4.04.

```sh
opam install ocamlbuild topkg ptime containers uri lwt webmachine qtest
ocaml pkg/pkg.ml build
ocaml pkg/pkg.ml test
```

### Docker ###

Alternatively it is also possible to build and run the application via Docker.
There is a [base-image][] which supplies the system. This repository contains a
`Dockerfile` which builds `reklama` and starts the web application.

```sh
docker build -t reklama:latest .
docker run -d -p 8080:8080 reklama:latest
```

Now you can access the REST API on port 8080.

## Usage ##

Specify your database in the `ads.sexp` file.

The `ads.sexp` file contains a list of ads to be served. Each ad consists of an
(integer) id (field `id`), a RFC 3339 compatible string containing a starting
time which signifies the starting time of an id to be served (field
`starting`). Simiarly it also contains a field `ending` which also includes a
RFC 3339 compatible string to determine which time is the latest time to be
queried. Next, there is a field `views` which takes an integer to determine
what the maximal amount of views it is. This includes views via a channel
matching als well as views via the ID directly. Then there is an `uri` which
determines what URL the REST service will redirect to in case the ad matches
the request. Then there are `categories` inherent to the ad itself, which will
cause a match if a user requests it with these interests. Finally, an ad has a
list of channels and views (that is how often an ad can be delivered for a
channel).

A channel consists of a string `name` and some `categories` (another string
list) inherent to the channel, which is coupled with the maximum amount of
views that are allowed for said channel. If the number reaches 0, the ad will
not be served to users requesting from this channel.

There is a command line interface (`_build/src/cli.native`) to request items,
as well as a REST based interface (`_build/src/web`) which will load the DB
upon startup and allow requesting over port 8080.

### Command line interface ###

You can call it and it asks you the channel you're coming from as well your
interests and matches you up with an ad. It does not have any meaningful
command line flags or anything.


### REST interface ###

The REST interface offers two endpoints, you can call them using [curl][] or
its cousin [httpie][].

```sh
http :8080/ad/23
```

#### `GET /ad/:id` ####

For looking up an ad directly by a known (integer) ID.

Return codes:

  * HTTP 307: Moved temporarily, when an ad was found which matches the ID and
    is also within views and time limit.
  * HTTP 410: Gone, when the ad was found but is over capacity or time
  * HTTP 404: Not found, when there is no such ad.

#### `GET /ad` #####

For looking up an ad by an originating channel and interests.

Query-Params:

  * `channel`: a string denoting the channel the user is coming from
  * `interests`: a list of strings (delimited by comma) of the interests of the
    user.

Return codes:

  * HTTP 307: Moved temporarily, when an ad was found which matches the ID,
    and is within views and time limit.
  * HTTP 410: Gone, when an ad was found but it is over capacity or time
  * HTTP 404: Not found, when no ads could be found which match

## License ##

See `LICENSE.md`. Spoiler alert: Apache-2.0.

[curl]: https://curl.haxx.se/
[httpie]: https://httpie.org/
[base-image]: https://hub.docker.com/r/leonidasfromxiv/docker-opam/
