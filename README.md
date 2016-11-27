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
opam install ocamlbuild topkg ptime containers uri lwt webmachine
ocaml pkg/pkg.ml build
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
