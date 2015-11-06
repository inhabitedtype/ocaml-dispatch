(*----------------------------------------------------------------------------
    Copyright (c) 2015 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

(** Path-based dispatching for js_of_ocaml applications.

    Dispatch_js makes it easy for js_of_ocaml applications to monitor and react
    to changes in URI fragments. *)

open Dispatch

val dispatch_on_fragment :
  ?on_failure:(string -> unit Lwt.t) -> ?default:string ->
  unit Lwt.t route list -> unit Lwt.t
(** [dispatch_on_fragment ?on_failure ?default routes] will monitor the URL
    fragment and dispatch to the appropriate hander in [routes]. In the event
    that the fragment does not match any routes, [on_failure] will be called,
    if provided, with the fragment. On setup the fragment will be set to
    [default], or ["/"] if no default is provided. *)

module DSL : sig
  open DSL 

  val dispatch_on_fragment : 
    ?on_failure:(string -> unit Lwt.t) -> ?default:string ->
    unit Lwt.t DSL.route list -> unit Lwt.t
  (** [dispatch_on_fragment ?on_failure ?default routes] is the same as the
      non-DSL version with the exception of the route type. *)
end
