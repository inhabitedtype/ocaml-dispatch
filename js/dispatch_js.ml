open Lwt
open Js_of_ocaml
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

let default_on_failure msg =
  print_endline msg;
  return_unit
;;

let dispatch_on_fragment ?(on_failure=default_on_failure) ?(default="/") routes =
  let dispatch_exn = Dispatch.dispatch_exn routes in
  let go frag =
    match dispatch_exn frag with
    | exception (Failure msg) -> on_failure msg
    | handler                 -> handler
  in
  let frag_loop =
    Lwt_js_events.onhashchanges (fun e _ ->
    let frag =
      let new_url = Js.to_string (e##.newURL) in
      match Url.url_of_string new_url with
      | Some (Url.Http u | Url.Https u) -> u.Url.hu_fragment
      | Some (Url.File u)               -> u.Url.fu_fragment
      | None                            ->
        if new_url = "" then default else assert false
    in
    go frag)
  in
  match Url.Current.get_fragment () with
  | "" ->
    Url.Current.set_fragment default;
    frag_loop
  | current ->
    go current >>= fun _ -> frag_loop
