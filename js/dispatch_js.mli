open Dispatch

val dispatch_on_fragment :
  ?on_failure:(string -> unit Lwt.t) ->
  ?default:string ->
  (assoc -> string option -> unit Lwt.t) route list -> unit Lwt.t
(** [dispatch_on_fragment ?on_failure ?default routes] will monitor the URL
    fragment and dispatch to the appropriate hander in [routes]. In the event
    that the fragment does not match any routes, [on_failure] will be called,
    if provided, with the fragment. On setup the fragment will be set to
    [default], or ["/"] if no default is provided. *)

module DSL : sig
  open DSL 

  val dispatch_on_fragment : 
    ?on_failure:(string -> unit Lwt.t) ->
    ?default:string ->
    (assoc -> string option -> unit Lwt.t) DSL.route list -> unit Lwt.t
  (** [dispatch_on_fragment ?on_failure ?default routes] is the same as the
      non-DSL version with the exception of the route type. *)
end
