Reklama
=======

Reklama is a proof of concept for serving ads

Building
--------

You need a system with [OPAM](https://opam.ocaml.org/) and a recent-ish OCaml
compiler, so something 4.02 or newer.

```bash
opam install ocamlbuild topkg ptime containers lwt webmachine
ocaml pkg/pkg.ml build
```

Usage
-----

Specify your database in the `ads.sexp` file.

There is a command line interface (`_build/src/cli.native`) to request items,
as well as a REST based interface (`_build/src/web`) which will load the DB
upon startup and allow requesting over port 8080.

License
-------

See `LICENSE.md`. Spoiler alert: Apache-2.0.
