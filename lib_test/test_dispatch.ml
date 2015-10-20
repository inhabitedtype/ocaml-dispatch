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

let base_path c _ _ = c
let param_path k ps _ = List.assoc k ps
let disp_path _ r =
  match r with
  | None   -> ""
  | Some r -> r

open OUnit
open Dispatch.DSL
open Result

let empty () =
  "an empty table will produce no result"
    @? begin match dispatch [] "/" with
       | Error _ -> true
       | _       -> false
    end

let single () =
  let table = ["/", (fun ps p -> ((), ps, p))] in
  "a single entry for the root will dispatch the root to it"
    @? begin match dispatch table "/" with
       | Ok((), [], None) -> true
       | _                -> false
    end;
  "a single entry for the root will not dispatch anything else to it"
    @? begin match dispatch table "/foo" with
       | Error _ -> true
       | _       -> false
    end

let overlap () =
  let table =
    [ ("/foo"    , base_path "/foo")
    ; ("/foo/bar", base_path "/foo/bar")
    ; ("/foo/baz", base_path "/foo/baz")
    ; ("/bar/baz", base_path "/bar/baz")
    ; ("/bar/foo", base_path "/bar/foo")
    ; ("/bar"    , base_path "/bar")
    ]
  in
  let open Dispatch.DSL in
  "a leading prefix pattern gets matched"
    @? begin match dispatch table "/foo" with
       | Ok "/foo" -> true
       | _         -> false
    end;
  "a trailing prefix pattern gets matched"
    @? begin match dispatch table "/bar" with
       | Ok "/bar" -> true
       | _         -> false
    end;
  "a complete pattern does not get shadowed by prefix"
    @? begin match dispatch table "/foo/baz" with
       | Ok "/foo/baz" -> true
       | _             -> false
    end;
;;

let keys () =
  let table =
    [ ("/foo/:id"         , param_path "id")
    ; ("/foo/:id/:bar"    , param_path "bar")
    ; ("/foo/:id/bar/:baz", param_path "baz")
    ]
  in
  "a leading prefix pattern gets matched and keys properly assigned"
    @? begin match dispatch table "/foo/1" with
       | Ok "1" -> true
       | _      -> false
    end;
  "a pattern with keys does not get shadowed by prefix"
    @? begin match dispatch table "/foo/1/test" with
       | Ok "test" -> true
       | _         -> false
    end;
  "a pattern with interleaved keys and literals works"
    @? begin match dispatch table "/foo/1/bar/one" with
       | Ok "one" -> true
       | _        -> false
    end;
;;

let wildcard () =
  let table = ["/foo/*", disp_path] in
  "a trailing wildcard pattern matches just the prefix"
    @? begin match dispatch table "/foo" with
       | Ok "" -> true
       | _     -> false
    end;
  "a trailing wildcard pattern matches a longer path"
    @? begin match dispatch table "/foo/bar/baz" with
       | Ok "bar/baz" -> true
       | _            -> false
    end;
;;

let rec was_successful =
  function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
        was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
        false

let _ =
  let tests = [
    "empty" >:: empty;
    "single" >:: single;
    "overlap" >:: overlap;
    "keys" >:: keys;
    "wildcard" >:: wildcard
  ] in
  let suite = (Printf.sprintf "test logic") >::: tests in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (was_successful (run_test_tt ~verbose:!verbose suite))
  then exit 1
