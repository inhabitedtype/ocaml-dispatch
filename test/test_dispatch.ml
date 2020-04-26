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
let params vs _ = vs
let param_path k ps _ = List.assoc k ps
let disp_path _ r =
  match r with
  | None   -> ""
  | Some r -> r

open Alcotest

let assoc = list (pair string string)

open Dispatch.DSL

let literals = 
  "literals", [
    "base cases", `Quick, begin fun () ->
      let t0, t1 = ["/", fun _ _ -> ()], ["", fun _ _ -> ()] in
      let check = Alcotest.check (option unit) in
      let test_ok  ~msg tbl p = check msg (dispatch tbl p) (Some ()) in
      let test_err ~msg tbl p = check msg (dispatch tbl p) None in
      test_err [] "/"     ~msg:"empty table produces errors";
      test_ok  t0 "/"     ~msg:"empty path string maps to root";
      test_ok  t1 ""      ~msg:"empty route path matches root";
      test_err t0 "/foo"  ~msg:"root entry won't dispatch others";
    end;
    "overlaping paths", `Quick, begin fun () ->
      let t0 =
        [ ("/foo"    , base_path "/foo")
        ; ("/foo/bar", base_path "/foo/bar")
        ; ("/foo/baz", base_path "/foo/baz")
        ; ("/bar/baz", base_path "/bar/baz")
        ; ("/bar/foo", base_path "/bar/foo")
        ; ("/bar"    , base_path "/bar")
        ]
      in
      let check = Alcotest.check (option string) in
      let test_ok ~msg p = check msg (dispatch t0 p) (Some p) in
      test_ok "/foo"      ~msg:"leading pattern gets matched";
      test_ok "/bar"      ~msg:"trailing pattern gets matched";
      test_ok "/foo/baz"  ~msg:"prefix match does not shadow";
    end
  ]

let params =
  "params", [
    "base cases", `Quick, begin fun () ->
      let t0 =
        [ ("/foo/:id"         , param_path "id")
        ; ("/foo/:id/:bar"    , param_path "bar")
        ; ("/foo/:id/bar/:baz", param_path "baz")
        ]
      in
      let check = Alcotest.check (option string) in
      let test_ok ~msg p v = check msg (dispatch t0 p) (Some v) in
      test_ok "/foo/1"          "1"   ~msg:"leading pattern matches";
      test_ok "/foo/1/test"    "test" ~msg:"prefix match does not shadow";
      test_ok "/foo/1/bar/one" "one"  ~msg:"interleaved keys and liters";
    end;
    "variable ordering", `Quick, begin fun () ->
      let t0 =
        [ ("/test/:z/:x/:y/"      , params)
        ; ("/test/:x/:y/order/:z/", params)
        ]
      in
      let check = Alcotest.check (option assoc) in
      let test_ok ~msg p v = check msg (dispatch t0 p) (Some v) in
      test_ok ~msg:"slashes not included in param"
        "/test/foo/bar/order/baz" ["x", "foo"; "y", "bar"; "z", "baz"];
      test_ok ~msg:"leading pattern matches"
        "/test/foo/bar/order"     ["z", "foo"; "x", "bar"; "y", "order"];
    end
  ]
;;

let wildcards =
  "wildcard", [
    let t0 = ["/foo/*", disp_path] in
    let check = Alcotest.check (option string) in
    let test_ok ~msg p v = check msg (dispatch t0 p) (Some v) in
    "base cases", `Quick, begin fun () ->
      test_ok ~msg: "a trailing wildcard pattern matches just the prefix"
        "/foo" "";
      test_ok ~msg:"a trailing wildcard pattern matches a longer path"
        "/foo/bar/baz" "bar/baz";
    end
  ]
;;

let () =
  Alcotest.run "Dispatch.DSL tests"
    [ literals; params; wildcards ]
