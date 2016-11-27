(** Reklama CLI *)

(* read input and do one single matching *)
let do_single_request db =
  (* Most of this function is a mess of IO *)
  print_string "Channel (Empty to quit): ";
  match read_line () with
  | "" -> None
  | ch ->
    (* like let-loop in Scheme or loop/recur in Clojure *)
    let rec loop interests =
      print_string "Interest (Empty to end): ";
      match read_line () with
        | "" -> interests
        | interest -> loop @@ interest::interests
    in
    let interests = loop [] in
    let current_time = Ptime_clock.now () in
    (* try to match an ad *)
    match Reklama.find_matching_ad db (Some ch) interests current_time with
    | None ->
        print_endline "No matches found";
        Some db
    | Some ad ->
        (* This will display the views with this one not yet counted, but as
           we can never access the fields in code it's not a problem since
           we never need an ad with the values, we just need it to hold the id *)
        Format.printf "Found %a\n" Reklama.Ad.print ad;
        let (uri, db) = Reklama.Ad.view (Some ch) ad db in
        (* Some printing of the proper result *)
        (match uri with
        | None -> ()
        | Some uri -> Format.printf "URI is %a\n" Uri.pp_hum uri);
        (* return the new db for the next round of requests *)
        Some db

let main () =
  (* Load ads *)
  let db = Reklama.load_initial_db "ads.sexp" in
  (* Loop till do_single_request stops returning a new db *)
  let rec loop = function
    | Some db -> loop @@ do_single_request db
    | None -> () in
  loop @@ Some db

(* In OCaml there is no main function per se, it executes everything in the
   top level of a file, like Python or Clojure. But this is a neat trick,
   it attempts to bind () (unit, kinda like void but an actual value) to
   `main ()` which only works when `main` returns (). *)
let () =
  main ()
