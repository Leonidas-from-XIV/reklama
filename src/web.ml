open Cohttp_lwt_unix
open Lwt.Infix

module Db = struct
end

module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix_io)
end

class ad db = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method private of_json rd =
    Wm.continue true rd

  method private to_json rd =
    Wm.continue (`String "{}") rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "application/json", self#of_json
    ] rd
end

let main () =
  let port = 8080 in
  let db = "todo" in
  let routes = [
    ("/ad/:id", fun () -> new ad db);
  ] in
  let callback (ch, conn) request body =
    let open Cohttp in
    Wm.dispatch' routes ~body ~request
    >|= (function
      | None -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result)
    >>= fun (status, headers, body, path) ->
      Server.respond ~headers ~body ~status ()
  in
  let config = Server.make ~callback () in
  Server.create ~mode:(`TCP(`Port port)) config
  >>= fun () ->
    Printf.eprintf "Listining on 0.0.0.0:%d\n" port;
    Lwt.return_unit

let () =
  Lwt_main.run @@ main ()
