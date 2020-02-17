open Guedra
open DrawModel
open Draw

type connect_type = ConnectLeft | ConnectRight

let rec make pid qid =

  let rec mk () =
    fun cmd ->
    match cmd with
      HitTest (xp, yp, k) -> k false (* cannot be selected *)
    | Draw (model, cap, oz, k) ->
       draw model cap k
    | MouseDown (xp, yp, bstate, button, model, cap, zo, k) ->
       draw_error "conn:MouseDown"
    | MouseMove (xp, yp, bstate, k) ->
       draw_error "conn:MouseMove"
    | MouseUp (xp, yp, bstate, button, k) ->
       draw_error "conn:MouseUp"
    | ConnectPoint (connect, k) ->
       draw_error "conn:ConnectPoint"
    | Boundary k -> k 0.0 0.0 0.0 0.0 (* ### *)
    | GetProp k -> k ()
    | SetProp (prop, k) -> k prop (mk ())
    | Move (dx, dy, k) -> k (mk ())
    | Resize (w, h, k) -> k (mk ())

  and draw model cap k =
    let pfig = get_act_fig model.fm cap pid in
    let qfig = get_act_fig model.fm cap qid in
    pfig
      (ConnectPoint
         (ConnectRight,
          (fun ct xp yp ->
            let (xp, yp) = offset_fig model.sel cap pid xp yp in
            qfig
              (ConnectPoint
                 (ConnectLeft,
                  (fun ct xq yq ->
                    let (xq, yq) = offset_fig model.sel cap qid xq yq in
                    set_color o_o.color_fore;
                    set_line_width 2.0;
                    draw_line xp yp xq yq;
                    k ()))))))

  in mk ()
