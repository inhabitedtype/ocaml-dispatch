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

(** Path-based dispatching for client- and server-side applications.

    Dispatch provides a basic mechanism for dispatching a request to a handler
    based on a conventionally heirarhical path name found in URIs. It can be
    used both for dispatching requests in a server, as well as handing changes
    to heirarchical fragments in a client-side application. *)

(** A segment of a path.

    A path is a sequence of segments separated by a '/' character. A segment
    can either be a literal string or any string that will be associated with
    the provided variable name. *)
module Segment : sig
  type t =
    | Lit of string
    | Var of string
end

type typ = [ `Prefix | `Exact ]
(** The type of match for the route. [`Prefix] indicates that the route does
    not need to match the entirety of the path, while [`Exact] indicates an
    exact match. *)

type assoc = (string * string) list
(** Type alias for an association list of [string] to [string] *)

type 'a route = Segment.t list * typ * (assoc -> string option -> 'a)
(** The type of a route. The first tuple element specifies the matching rules
    for the path components, with the tag indicating the type of match. The
    interpretation of the corresponding string depends on the tag provided: for
    [(`Lit, lit)], the path component must match the string [lit] exactly; for
    [(`Var, name)], the path component can be anything and will be associated
    with [name] in the event of a successful match.

    The second component of the tuple indicates the type of match that this
    route support, which can be either a prefix match (indicated by the
    [`Prefix] variant), or an exact match (indicated by the [`Exact] variant).

    The third and final component is the handler function. On a successful
    route match, the matching information will be passed to the handler to
    produce a value of tyoe ['a] that will be returned. *)

type 'a t

val create : 'a route list -> 'a t

val dispatch     : 'a t -> string -> 'a option
val dispatch_exn : 'a t -> string -> 'a
(** [dispatch routes path] iterates through [routes] and selects the first one
    that matches [path]. It then applies the route handler to any component
    mappings and trailing path components (in the case of a prefix match) and
    returns the result. If none of the [routes] matches [path], it will return
    [None].

    [dispatch_exn routes path] behaves just like [dispatch routes path] except
    will raise an exception using [failwith] in the case of no matches. *)

val to_dsl : Segment.t list * typ -> string
val of_dsl : string -> Segment.t list * typ
(** [to_dsl route_spec] is a string in the routing DSL that will beahve in the
    exact same way as [route_spec].

    A good way to become comfortable with the DSL is to load up this library in
    the REPL of your choice and translate route patterns to the DSL using the
    function [to_dsl]. *)

(** A module that implements the dispatch operations for a DLS represented as
    a string literal. A more familiar interface for the Web world. *)
module DSL : sig
  type 'a route = string * (assoc -> string option -> 'a)
  (** The type of a route using the DSL to specify the path pattern. For
      example, here are some DSL strings and their translation:

  {v   # of_dsl "/";;
    = ([], `Exact)
  # of_dsl "/user/:id";;
    = ([`Lit, "user"; `Var, "id"], `Exact)
  # of_dsl "/user/:id/*";;
    = ([`Lit, "user"; `Var, "id"], `Prefix)
  # of_dsk "/user/:id/settings";;
    = ([`Lit, "user"; `Var, "id"; `Lit, "settings"], `Exact) v} *)

  val create : 'a route list -> 'a t
end
