open Cohttp_lwt_unix
open Lwt.Infix

module Db = struct
end

module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix_io)
end


