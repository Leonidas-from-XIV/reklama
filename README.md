Reklama
=======

Reklama is a proof of concept for serving ads

Building
--------

You need a system with [OPAM](https://opam.ocaml.org/) and a recent-ish OCaml
compiler, so something 4.02 or newer.

```bash
opam install ocamlbuild topkg containers
ocaml pkg/pkg.ml -build
```

Usage
-----

TODO. Currently there is a command line tool but that's not the end of the line
yet.

Specify your database in the `ads.sexp` file.

License
-------

See `LICENSE.md`. Spoiler alert: Apache-2.0.
