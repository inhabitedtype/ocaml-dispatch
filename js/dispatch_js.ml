open Lwt
open Result

let dispatch_on_fragment ?on_failure ?(default="/") routes = 
  let dispatch = Dispatch.dispatch_apply routes in
  let on_failure =
    match on_failure with
    | None   -> (fun msg -> print_endline msg; return_unit)
    | Some f -> f
  in
  let go frag =
    match dispatch frag with
    | Error msg -> on_failure msg
    | Ok    res -> res
  in
  let frag_loop =
    Lwt_js_events.onhashchanges (fun e _ ->
    let frag =
      let new_url = Js.to_string (e##newURL) in
      match Url.url_of_string new_url with
      | Some (Url.Http u | Url.Https u) -> u.Url.hu_fragment
      | Some (Url.File u)               -> u.Url.fu_fragment
      | None                            ->
        if new_url = "" then default else assert false
    in
    go frag)
  in
  let current = Url.Current.get_fragment () in
  if current = "" then begin 
    Url.Current.set_fragment default;
    frag_loop
  end else 
    go current >>= fun _ -> frag_loop

module DSL = struct
  let convert routes =
    List.map (fun (m, x) ->
      let ts, t = Dispatch.of_dsl m in
      ts, t, x)
    routes

  let dispatch_on_fragment ?on_failure ?default routes =
    dispatch_on_fragment ?on_failure ?default (convert routes)
end
