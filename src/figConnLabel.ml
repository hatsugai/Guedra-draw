open Guedra
open DrawModel
open Draw

let rec make pid qid label dx dy =

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
         (FigConn.ConnectRight,
          (fun ct xp yp ->
            let (xp, yp) = offset_fig model.sel cap pid xp yp in
            qfig
              (ConnectPoint
                 (FigConn.ConnectLeft,
                  (fun ct xq yq ->
                    let (xq, yq) = offset_fig model.sel cap qid xq yq in
                    set_color o_o.color_fore;
                    set_line_width 2.0;
                    draw_line xp yp xq yq;
                    set_font o_o.font;
                    let extents = text_extents label in
                    let x = (xp +. xq) *. 0.5 in
                    let y = (yp +. yq) *. 0.5 in
                    set_color o_o.color_back;
                    fill_rect
                      (x +. dx -. extents.width *. 0.5 -. o_o.left_margin)
                      (y +. dy -. extents.height *. 0.5 -. o_o.top_margin)
                      (extents.width +. o_o.left_margin +. o_o.right_margin)
                      (extents.height +. o_o.top_margin +. o_o.bottom_margin);
                    set_color o_o.color_fore;
                    draw_text
                      (x +. dx -. extents.width *. 0.5)
                      (y +. dy -. extents.height *. 0.5)
                      label;
                    k () ))))))

  in mk ()
