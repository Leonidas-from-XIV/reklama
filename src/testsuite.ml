(* Here we do some testing. The way we do so is to use generative/property
 * based testing, like Haskell's QuickCheck or Clojure's test.check *)
open Containers

(* First, we need some generators *)

let ad_id = 23

let channel = Reklama.{
    name = "nyt";
    categories = []
  }

let uri = Uri.of_string "https://nytimes.com/"

(* Generates a random tuple of (views, Ad.t), where views is a small integer and
   the ad has a channel definition with half as many possible views as the global view
   counter.  *)
let ad = QCheck.(
  small_int
  |> map @@ fun views ->
    (views,
     Reklama.Ad.build ad_id Ptime.min Ptime.max views uri [(channel, views / 2)] ["travel"]))

(* Generates an ad which may or may not have a travel interest *)
let ad_maybe_with_interest = QCheck.(
  (pair pos_int bool) |> map @@ fun (id, include_travel) ->
    Reklama.Ad.build id Ptime.min Ptime.max max_int uri [] (if include_travel then ["travel"] else []))

(* Generate a list of these ads, by using the list combinator *)
let ads_with_interests = QCheck.list ad_maybe_with_interest

(* For convenience, convert the list directly into an Ad.DataBase.t *)
let db_with_interests = QCheck.(ads_with_interests |> map @@ fun ads -> Reklama.Ad.db_of_ad_list ads)

(* Make sure that we can't retrieve more views from an ad than the Ad had configured *)
let proper_amount_of_views = QCheck.(Test.make ~count:1000 ad @@
  fun (views, ad) ->
    (* Here we got the number of views and the ad, build a db for querying *)
    let db = Reklama.Ad.db_of_ad_list [ad] in
    (* Count views till the find function does not return any more URIs *)
    let rec loop db uris =
      match Reklama.find_ad_by_id db (Ptime_clock.now ()) ad_id with
      | None -> uris
      | Some ad ->
          match Reklama.Ad.view None ad db with
          | (None, _) -> uris
          | (Some uri, db) -> loop db (uri::uris)
    in
    let uris = loop db [] in
    (* Check whether we got exactly as many URIs as the Ad was supposed to have views *)
    List.length uris = views)

(* Make sure that viewing from a channel returns exactly the configured amount of channels *)
let proper_amount_of_channel_views = QCheck.(Test.make ~count:1000 ad @@
  fun (views, ad) ->
    (* We know how many views we expect *)
    let channel_views = views / 2 in
    (* The channel name is always the same *)
    let channel_name = Some Reklama.(channel.name) in
    let db = Reklama.Ad.db_of_ad_list [ad] in
    (* Count views till no more views are returned *)
    let rec loop db uris =
      match Reklama.find_matching_ad db channel_name ["travel"] (Ptime_clock.now ()) with
      | None -> uris
      | Some ad ->
          match Reklama.Ad.view channel_name ad db with
          | (None, _) -> uris
          | (Some uri, db) -> loop db (uri::uris)
    in
    let uris = loop db [] in
    (* Check whether we got exactly as many views for a channel as we were supposed to get *)
    List.length uris = channel_views)

(* Make sure all the matches that we got have the proper category match *)
let all_matches_proper_interest = QCheck.(Test.make ~count:1000 db_with_interests @@
  fun db ->
    let channel_name = Some Reklama.(channel.name) in
    (* Collect the categories of all ads which we got matched *)
    (* This function needs a counter, since the number of views permissible is virtually unbounded *)
    let rec loop db categories = function
      | 0 -> categories
      | n ->
        match Reklama.find_matching_ad db channel_name ["travel"] (Ptime_clock.now ()) with
        | None -> categories
        | Some ad ->
            match Reklama.Ad.view channel_name ad db with
            | (None, _) -> categories
            | (_, db) -> loop db ((Reklama.Ad.categories channel_name ad)::categories) (n-1)
    in
    let categories = loop db [] 100 in
    (* helper fn to see whether an element has at least one "travel" category *)
    let has_travel_category categories =
      categories
      |> Reklama.Categories.to_list
      |> List.exists (fun x -> x = "travel")
    in
    (* Make sure all hits have at least a travel category, as requested *)
    List.for_all has_travel_category categories)

let main () =
  (* All the checks that we have and which should be run *)
  QCheck_runner.run_tests_main [
    proper_amount_of_views;
    proper_amount_of_channel_views;
    all_matches_proper_interest;
  ]

let () =
  main ()
