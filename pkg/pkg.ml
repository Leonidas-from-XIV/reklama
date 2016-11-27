#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe
    ~change_logs:[]
    ~opams:[Pkg.opam_file ~lint_deps_excluding:(Some ["qtest"; "qcheck"]) "opam"]
    "reklama" @@ fun c ->
  Ok [ Pkg.bin "src/cli";
       Pkg.bin "src/web";
       Pkg.test "src/testsuite" ]
