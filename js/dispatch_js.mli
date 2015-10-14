open Dispatch

val dispatch_on_fragment :
  ?on_failure:(string -> unit Lwt.t) ->
  ?default:string ->
  (assoc -> string option -> unit Lwt.t) route list -> unit Lwt.t

module DSL : sig
  open DSL 

  val dispatch_on_fragment : 
    ?on_failure:(string -> unit Lwt.t) ->
    ?default:string ->
    (assoc -> string option -> unit Lwt.t) DSL.route list -> unit Lwt.t
end
