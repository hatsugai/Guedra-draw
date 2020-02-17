open Guedra
open DrawModel
open Draw

type handle = LT | MT | RT | LM | RM | LB | MB | RB

type transform_context =
  Idle
| Moving of float * float * float * float
| Resizing of handle * float * float * float * float

let handles = [LT; MT; RT; LM; RM; LB; MB; RB]

let handle_point handle x y w h =
  match handle with
    LT -> (x, y)
  | MT -> (x +. w *. 0.5, y)
  | RT -> (x +. w, y)
  | LM -> (x, y +. h *. 0.5)
  | RM -> (x +. w, y +. h *. 0.5)
  | LB -> (x, y +. h)
  | MB -> (x +. w *. 0.5, y +. h)
  | RB -> (x +. w, y +. h)

let in_handle x y xp yp =
  xp >= x -. o_o.handle_size && xp < x +. o_o.handle_size *. 2.0 &&
    yp >= y -. o_o.handle_size && yp < y +. o_o.handle_size *. 2.0

let hittest_handle x y w h xp yp =
  List.find_opt
    (fun handle ->
      let (xh, yh) = handle_point handle x y w h in
      in_handle xh yh xp yp)
  handles

let transform_by_handle handle x y w h dx dy =
  match handle with
    LT -> (x +. dx, y +. dy, w -. dx, h -. dy)
  | MT -> (x, y +. dy, w, h -. dy)
  | RT -> (x, y +. dy, w +. dx, h -. dy)
  | LM -> (x +. dx, y, w -. dx, h)
  | RM -> (x, y, w +. dx, h)
  | LB -> (x +. dx, y, w -. dx, h +. dy)
  | MB -> (x, y, w, h +. dy)
  | RB -> (x, y, w +. dx, h +. dy)

let rec make_figure_function id x y w h tc0 =

  let rec mk tc =
    fun cmd ->
    match cmd with
      HitTest (xp, yp, k) ->
       k (hittest xp yp)
    | Draw (model, cap, oz, k) ->
       draw tc model cap k
    | MouseDown (xp, yp, state, button, model, cap, zo, k) ->
       if (state land keyStateShift) = 0 then
         let fig = mouse_down tc xp yp in
         let model' = { model with sel = IntSet.singleton id } in
         k model' (CapFigure (id, fig)) []
       else
         if IntSet.mem id model.sel then
           let model' = { model with sel = IntSet.remove id model.sel } in
           k model' CapNone []
         else
           let model' = { model with sel = IntSet.add id model.sel } in
           k model' CapNone []
    | MouseMove (xp, yp, state, k) ->
       k (mouse_move tc xp yp)
    | MouseUp (xp, yp, state, button, k) ->
       k (mouse_up tc xp yp)
    | ConnectPoint (connect, k) ->
       connect_point tc connect k
    | Boundary k -> k x y w h
    | GetProp k -> k ()
    | SetProp (prop, k) -> k prop (mk tc)
    | Move (dx, dy, k) ->
       k (make_figure_function id (x +. dx) (y +. dy) w h tc)
    | Resize (w, h, k) ->
       k (make_figure_function id x y w h tc)

  and transform tc =
    let (x, y, w, h) =
      match tc with
        Idle -> (x, y, w, h)
      | Moving (x0, y0, x1, y1) ->
         (x +. x1 -. x0, y +. y1 -. y0, w, h)
      | Resizing (handle, x0, y0, x1, y1) ->
         transform_by_handle handle x y w h (x1 -. x0) (y1 -. y0)
    in
    let (x, w) = if w >= 0.0 then (x, w) else (x +. w, -. w) in
    let (y, h) = if h >= 0.0 then (y, h) else (y +. h, -. h) in
    (x, y, w, h)

  and connect_point tc connect_type k =
    let (x, y, w, h) = transform tc in (* ### *)
      match connect_type with
        FigConn.ConnectLeft  -> k connect_type x (y +. h *. 0.5)
      | FigConn.ConnectRight -> k connect_type (x +. w) (y +. h *. 0.5)

  and hittest xp yp =
    match hittest_handle x y w h xp yp with
      None ->
      xp >= x && xp < x +. w && yp >= y && yp < y +. h
    | Some handle -> true

  and mouse_down tc xp yp =
    flush stdout;
    match hittest_handle x y w h xp yp with
      None ->
       mk (Moving (xp, yp, xp, yp))
    | Some handle ->
       mk (Resizing (handle, xp, yp, xp, yp))

  and mouse_move tc xp yp =
    match tc with
      Moving (x0, y0, _, _) ->
       mk (Moving (x0, y0, xp, yp))
    | Resizing (handle, x0, y0, x1, y1) ->
       mk (Resizing (handle, x0, y0, xp, yp))
    | Idle -> draw_error "mouse_move"

  and mouse_up tc xp yp =
    let (x, y, w, h) = transform tc in
    make_figure_function id x y w h Idle

  and draw tc model cap k =
    let (x, y, w, h) = transform tc in
    let (x, y) =
      match cap with
        CapMove ((x0, y0), (x1, y1)) ->
         if IntSet.mem id model.sel then
           (x +. x1 -. x0, y +. y1 -. y0)
         else (x, y)
      | _ -> (x, y)
    in
    set_color o_o.color_back;
    fill_rect x y w h;
    set_color o_o.color_fore;
    set_line_width 2.0;
    draw_rect x y w h;
    (match cap with
       CapNone ->
        (if IntSet.mem id model.sel then
           (if IntSet.is_single model.sel then
              draw_handles x y w h
            else
              draw_sel_handles x y w h))
     | CapFigure (id, fig) -> ()
     | CapScroll _ -> ()
     | CapSelect ((x0, y0), (x1, y1), tsel) ->
        if IntSet.mem id tsel then
          (if IntSet.is_single model.sel then
             draw_handles x y w h
           else
             draw_sel_handles x y w h)
        else ()
     | CapMove ((x0, y0), (x1, y1)) -> ());
    k ()

  in mk tc0

let make id x y w h =
  make_figure_function id x y w h Idle
