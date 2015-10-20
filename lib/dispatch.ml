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

open Result

type tag =
  [ `Lit | `Var ]

type typ =
  [ `Prefix | `Exact ]

type assoc = (string * string) list
type 'a route = (tag * string) list * typ * 'a

let path_split path =
  (* NOTE(seliopou): This was implemented manually to minimize dependencies for
   * js_of_ocaml. Ain't nobody got time for another regular expression library
   * in their browser. *)
  let rec loop i acc =
    try
      let j = String.index_from path i '/' in
      loop (j + 1) (String.(sub path i (j - i))::acc)
    with Not_found ->
      let len = String.length path in
      let result =
        if i >= len
          then acc
          else (String.sub path i (len - i))::acc
      in
      List.rev result
  in
  match path with
  | "" -> []
  | _ ->
    if String.get path 0 = '/'
      then loop 1 []
      else loop 0 []

let to_dsl (ms, typ) =
  let start =
    String.concat "/" (List.map (function
      | (`Lit, x) -> x
      | (`Var, x) -> ":" ^ x)
    ms)
  in
  match typ with
  | `Exact  -> start
  | `Prefix -> start ^ "/*"

let of_dsl str =
  let star, rev_parts =
    match List.rev (path_split str) with
    | "*"::ps' -> `Prefix, ps'
    | ps'      -> `Exact , ps'
  in
  let parts =
    List.fold_left (fun acc p ->
      let len = String.length p in
      if len > 0 && String.get p 0 = ':' then
        (`Var, String.sub p 1 (len - 1))::acc
      else
        (`Lit, p)::acc)
    [] rev_parts
  in
  parts, star

let path_match ps0 ms0 =
  let rec loop ps ms acc =
    match ps, ms with
    | []    , []  -> `Exact (List.rev acc)
    | _     , []  -> `Partial (List.rev acc, ps)
    | []    , _   -> `Failure (Printf.sprintf
      "unmatched pattern suffix: %s" (to_dsl (ms, `Exact)))
    | p::ps', (`Lit, l)::ms' ->
      if p = l then loop ps' ms' acc else `Failure (Printf.sprintf
        "pattern mismatch: expected '%s' but got '%s'" l p)
    | p::ps', (`Var, m)::ms' ->
      loop ps' ms' ((m, p) :: acc)
  in
  loop ps0 ms0 []

let dispatch routes path =
  let ps0 = path_split path in
  let rec loop = function
    | []          -> Error "no matching routes found"
    | (ms, exact, f)::xs ->
      begin match exact, path_match ps0 ms with
      | #typ   , `Exact assoc        -> Ok(f assoc None)
      | `Prefix, `Partial(assoc, ps) -> Ok(f assoc (Some (String.concat "/" ps)))
      | `Exact , `Partial _          -> loop xs
      | _      , `Failure _          -> loop xs
      end
  in
  loop routes

let dispatch_exn routes path =
  match dispatch routes path with
  | Ok x      -> x
  | Error msg -> failwith msg

module DSL = struct
  type 'a route = string * 'a

  let convert routes =
    List.map (fun (m, x) ->
      let ts, t = of_dsl m in
      ts, t, x)
    routes

  let dispatch routes =
    dispatch (convert routes)

  let dispatch_exn routes =
    dispatch_exn (convert routes)
end
