#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe
    ~change_logs:[]
    "reklama" @@ fun c ->
  Ok [ Pkg.bin "src/cli";
       Pkg.bin "src/web";
       Pkg.test "src/testsuite" ]
