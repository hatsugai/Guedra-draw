open Csp
open Guedra
open DrawModel

type ('a, 'b, 'c) context = {
    mutable width : int;
    mutable height : int;
    mutable fWidth : float;
    mutable fHeight : float;
    mutable inv : event option;
    mutable model : ('a, 'b, 'c) model;
    mutable cap : ('a, 'b, 'c) capture;
    oz : oz;
  }

let color_selectbox = color 0.0 1.0 0.0

let draw_handle x y =
  set_color o_o.color_handle;
  fill_rect
    (x -. o_o.handle_size) (y -. o_o.handle_size)
    (o_o.handle_size *. 2.0) (o_o.handle_size *. 2.0)

let draw_handle_sel x y =
  set_color color_white;
  fill_rect
    (x -. o_o.handle_size) (y -. o_o.handle_size)
    (o_o.handle_size *. 2.0) (o_o.handle_size *. 2.0);
  set_color o_o.color_handle;
  draw_rect
    (x -. o_o.handle_size) (y -. o_o.handle_size)
    (o_o.handle_size *. 2.0) (o_o.handle_size *. 2.0)

let draw_handles x y w h =
  draw_handle x y;
  draw_handle (x +. w) y;
  draw_handle x (y +. h);
  draw_handle (x +. w) (y +. h);
  draw_handle (x +. w *. 0.5) y;
  draw_handle x (y +. h *. 0.5);
  draw_handle (x +. w *. 0.5) (y +. h);
  draw_handle (x +. w) (y +. h *. 0.5)

let draw_sel_handles x y w h =
  draw_handle_sel x y;
  draw_handle_sel (x +. w) y;
  draw_handle_sel x (y +. h);
  draw_handle_sel (x +. w) (y +. h);
  draw_handle_sel (x +. w *. 0.5) y;
  draw_handle_sel x (y +. h *. 0.5);
  draw_handle_sel (x +. w *. 0.5) (y +. h);
  draw_handle_sel (x +. w) (y +. h *. 0.5)

(* screen to figure *)
let transform oz x y =
  (float_of_int (x - oz.ox) /. oz.scale, float_of_int (y - oz.oy) /. oz.scale)

let fig_to_scn oz x y w h =
  (int_of_float (x *. oz.scale) + oz.ox,
   int_of_float (y *. oz.scale) + oz.oy,
   int_of_float (w *. oz.scale),
   int_of_float (h *. oz.scale))

let push_clip_f x y w h =
  push_clip (int_of_float x) (int_of_float y) (int_of_float w) (int_of_float h)

let hittest x y fz fm cont =
  IntMap.enum_lnr () fz
    (fun _ p_opt k ->
      match p_opt with
        None -> cont None
      | Some (zo, id) ->
         let fig = IntMap.find id fm in
         fig (HitTest (x, y,
                       (fun b ->
                         if b then
                           cont p_opt
                         else
                           k ()))))

let draw_figures model cap oz cont =
  match cap with
    CapFigure (id, fig) ->
     IntMap.enum_rnl () model.fz
       (fun _ p_opt next ->
         match p_opt with
           None -> cont ()
         | Some (zo, id') ->
            let fig =
              if id' = id then fig
              else IntMap.find id' model.fm in
            fig (Draw (model, cap, oz, next)))
  | _ ->
     IntMap.enum_rnl () model.fz
       (fun _ p_opt next ->
         match p_opt with
           None -> cont ()
         | Some (zo, id) ->
            let fig = IntMap.find id model.fm in
            fig (Draw (model, cap, oz, next)))

let rect_corner x0 y0 x1 y1 =
  if x0 <= x1 then
    if y0 <= y1 then
      rect x0 y0 (x1 -. x0) (y1 -. y0)
    else
      rect x0 y1 (x1 -. x0) (y0 -. y1)
  else
    if y0 <= y1 then
      rect x1 y0 (x0 -. x1) (y1 -. y0)
    else
      rect x1 y1 (x0 -. x1) (y0 -. y1)

let cross r x y w h =
  not (x +. w <= r.x || r.x +. r.w <= x 
       || y +. h <= r.y || r.y +. r.h <= y)

let select_figures fm x0 y0 x y k =
  let r = rect_corner x0 y0 x y in
  IntMap.enum_lnr IntSet.empty fm
    (fun tsel o next ->
      match o with
        None -> k tsel
      | Some (id, fig) ->
         fig
           (Boundary
              (fun x y w h ->
                if cross r x y w h then
                  next (IntSet.add id tsel)
                else
                  next tsel)))

let move_figures fm sel dx dy k =
  IntMap.enum_lnr fm fm
    (fun fm o next ->
      match o with
        None -> k fm
      | Some (id, fig) ->
         if IntSet.mem id sel then
           fig
             (Move
                (dx, dy,
                 (fun fig ->
                   next (IntMap.add id fig fm))))
         else next fm)


(* 選択図形の境界を求める *)
let boundary_fig_set fm s k =
  IntSet.enum_lnr (1.0e308, 1.0e308, -1.0e308, -1.0e308) s
    (fun (x0, y0, x1, y1) o next ->
      match o with
        None -> k x0 y0 (x1 -. x0) (y1 -. y0)
      | Some id ->
         let fig = IntMap.find id fm in
         fig
           (Boundary
              (fun x y w h ->
                next (min x x0, min y y0, max (x +. w) x1, max (y +. h) y1))))

let boundary_fig_all fm k =
  IntMap.enum_lnr (1.0e308, 1.0e308, -1.0e308, -1.0e308) fm
    (fun (x0, y0, x1, y1) o next ->
      match o with
        None -> k x0 y0 (x1 -. x0) (y1 -. y0)
      | Some (id, fig) ->
         fig
           (Boundary
              (fun x y w h ->
                next (min x x0, min y y0, max (x +. w) x1, max (y +. h) y1))))

let adjust_origin fm s width height oz k =
  let (x0, y0) = transform oz 0 0 in
  let (x1, y1) = transform oz width height in
  boundary_fig_set fm s
    (fun x y w h ->
      if x1 <= x || x +. w <= x0 || y1 <= y || y +. h <= y0 then
        k (width / 2 - int_of_float ((x +. w *. 0.5) *. oz.scale),
           height / 2 - int_of_float ((y +. h *. 0.5) *. oz.scale))
      else
        k (oz.ox, oz.oy))

let right_most fm width height oz k =
  IntMap.enum_lnr (0.0, 0.0, 0.0, 0.0, -1) fm
    (fun (x, y, w, h, id) o next ->
      match o with
        None ->
         k (width / 2 - int_of_float ((x +. w *. 0.5) *. oz.scale),
            height / 2 - int_of_float ((y +. h *. 0.5) *. oz.scale))
      | Some (id', fig) ->
         fig
           (Boundary
              (fun x' y' w' h' ->
                if x < x' then
                  next (x', y', w', h', id')
                else
                  next (x, y, w, h, id))))

let init wch pch cch nch ext =
  let pque = Queue.create () in
  let nque = Queue.create () in
  let cc = {
      width = 0;
      height = 0;
      fWidth = 0.0;
      fHeight = 0.0;
      inv = None;
      model = {
          fz = IntMap.empty;
          fm = IntMap.empty;
          sel = IntSet.empty;
          ext = ext;
        };
      cap = CapNone;
      oz = { ox = 0; oy = 0; scale = 1.0; };
    }
  in

  let rec process () =
    let event_list = [
        recvEvt wch always win_msg;
        recvEvt cch always win_cmd
      ]
    in
    select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
      Paint (x, y, w, h) -> paint x y w h
    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       cc.fWidth <- float_of_int w;
       cc.fHeight <- float_of_int h;
       process ()
    | MouseDown (x, y, state, button) ->
       mouse_down x y state button
    | MouseUp (x, y, state, button) ->
       mouse_up x y state button
    | MouseMove (x, y, state) ->
       mouse_move x y state
    | MouseWheel (x, y, state, direction) ->
       mouse_wheel x y state direction
    | Scroll (x, y, state, dx, dy) ->
       scroll x y state dx dy
    | Scale scale ->
       let x = cc.oz.scale *. scale in
       if x >= 0.1 && x <= 4.0 then
         (cc.oz.scale <- x; invalidate ())
       else
         process ();
    | KeyDown (code, state) ->
       notify (OnKeyDown (code, state));
       process ()
    | _ -> process ()

  and paint x y w h =
    set_color o_o.color_back;
    fill_rect 0.0 0.0 cc.fWidth cc.fHeight;
    (match cc.cap with
       CapNone ->
        push_translate_scale (flo cc.oz.ox) (flo cc.oz.oy) cc.oz.scale
     | CapFigure _ ->
        push_translate_scale (flo cc.oz.ox) (flo cc.oz.oy) cc.oz.scale
     | CapScroll (x0, y0, x1, y1) ->
        push_translate_scale (flo (cc.oz.ox + x1 - x0))
          (flo (cc.oz.oy + y1 - y0)) cc.oz.scale
     | CapSelect ((x0, y0), (x1, y1), tsel) ->
        push_translate_scale (flo cc.oz.ox) (flo cc.oz.oy) cc.oz.scale;
        set_color color_selectbox;
        draw_rect x0 y0 (x1 -. x0) (y1 -. y0)
     | CapMove _ ->
        push_translate_scale (flo cc.oz.ox) (flo cc.oz.oy) cc.oz.scale);
    draw_figures cc.model cc.cap cc.oz
      (fun () ->
        pop_translate_scale ();
        send pch (wch, PaintAck) process)

  and mouse_down x y state button =
    request Activate;
    if button = mouseButtonLeft then (
      let (xf, yf) = transform cc.oz x y in
      hittest xf yf cc.model.fz cc.model.fm
        (fun p_opt ->
          match p_opt with
            None ->
             (if (state land keyStateShift) = 0
                 && not (IntSet.is_empty cc.model.sel)
              then         (* unselect *)
                (cc.model <- { cc.model with sel = IntSet.empty; };
                 notify (OnSelected cc.model)));
             cc.cap <- CapSelect ((xf, yf), (xf, yf), cc.model.sel);
             invalidate ()
          | Some (zo, id) ->
             if (state land keyStateMask) = 0
                && IntSet.mem id cc.model.sel
                && not (IntSet.is_single cc.model.sel)
             then
               (cc.cap <- CapMove ((xf, yf), (xf, yf));
                process ())
             else
               deliver_mouse_down xf yf state button zo id))
    else if button = mouseButtonRight then
      (if (state land keyStateMask) = 0 then
         (cc.cap <- CapScroll (x, y, x, y);
          process ())
       else
         hittest_and_deliver_mouse_down x y state button)
    else
      hittest_and_deliver_mouse_down x y state button

  and mouse_up x y state button =
    match cc.cap with
      CapNone -> process ()
    | CapFigure (id, fig) ->
       let (xf, yf) = transform cc.oz x y in
       fig (MouseUp (xf, yf, state, button,
                     (fun fig' ->
                       let model = cc.model in
                       cc.model <-
                         { model with
                           fm = IntMap.add id fig' cc.model.fm };
                       notify (OnChanged (model, cc.model));
                       cc.cap <- CapNone;
                       invalidate ())))
    | CapScroll (x0, y0, _, _) ->
       cc.oz.ox <- cc.oz.ox + x - x0;
       cc.oz.oy <- cc.oz.oy + y - y0;
       cc.cap <- CapNone;
       invalidate ()
    | CapSelect ((x0, y0), _, tsel) ->
       let (xf, yf) = transform cc.oz x y in
       select_figures cc.model.fm x0 y0 xf yf
         (fun tsel ->
           cc.cap <- CapNone;
           let sel = IntSet.union cc.model.sel tsel in
           (if sel <> cc.model.sel then
              (cc.model <- { cc.model with sel = sel; };
               notify (OnSelected cc.model)));
           invalidate ())
    | CapMove ((x0, y0), _) ->
       cc.cap <- CapNone;
       let (xf, yf) = transform cc.oz x y in
       let dx = xf -. x0 and dy = yf -. y0 in
       if dx *. dx +. dy *. dy <= o_o.mouse_dist_threshold /. cc.oz.scale then
         invalidate()
       else
         move_figures cc.model.fm cc.model.sel dx dy
           (fun fm ->
             let model = cc.model in
             cc.model <- { cc.model with fm = fm };
             notify (OnChanged (model, cc.model));
             invalidate ())

  and mouse_move x y state =
    match cc.cap with
      CapNone -> process ()
    | CapFigure (id, fig) ->
       let (xf, yf) = transform cc.oz x y in
       fig (MouseMove (xf, yf, state,
                       (fun fig' ->
                         cc.cap <- CapFigure (id, fig');
                         invalidate ())))
    | CapScroll (x0, y0, _, _) ->
       cc.cap <- CapScroll (x0, y0, x, y);
       invalidate ()
    | CapSelect ((x0, y0), _, tsel) ->
       let (xf, yf) = transform cc.oz x y in
       select_figures cc.model.fm x0 y0 xf yf
         (fun tsel ->
           cc.cap <- CapSelect ((x0, y0), (xf, yf), tsel);
           invalidate ())
    | CapMove ((x0, y0), _) ->
       let (xf, yf) = transform cc.oz x y in
       cc.cap <- CapMove ((x0, y0), (xf, yf));
       invalidate ()

  and mouse_wheel x y state direction =
    if (state land keyStateMask) = keyStateControl then (
      if direction > 0 then
        ((if cc.oz.scale < 10.0 then
            let (xf, yf) = transform cc.oz x y in
            cc.oz.scale <- cc.oz.scale *. 1.125;
            cc.oz.ox <- x - int_of_float (xf *. cc.oz.scale);
            cc.oz.oy <- y - int_of_float (yf *. cc.oz.scale));
         invalidate ())
      else
        ((if cc.oz.scale > 0.1 then
            let (xf, yf) = transform cc.oz x y in
            cc.oz.scale <- cc.oz.scale /. 1.125;
            cc.oz.ox <- x - int_of_float (xf *. cc.oz.scale);
            cc.oz.oy <- y - int_of_float (yf *. cc.oz.scale));
         invalidate ()))
    else
      process ()

  and scroll x y state dx dy =
    if (state land keyStateMask) = 0 then
      (cc.oz.ox <- cc.oz.ox + dx;
       cc.oz.oy <- cc.oz.oy + dy;
       invalidate ())
    else if (state land keyStateControl) = 0 then
      let (xf, yf) = transform cc.oz x y in
      hittest xf yf cc.model.fz cc.model.fm
        (fun p_opt ->
          match p_opt with
            None -> process ()
          | Some (zo, id) ->
             let dxf = (flo dx) /. cc.oz.scale in
             let dyf = (flo dy) /. cc.oz.scale in
             notify (OnScroll (cc.model, id, state, dxf, dyf));
             process ())
    else
      process ()

  and hittest_and_deliver_mouse_down x y state button =
    let (xf, yf) = transform cc.oz x y in
    hittest xf yf cc.model.fz cc.model.fm
      (fun p_opt ->
        match p_opt with
          None -> process ()
        | Some (zo, id) ->
           deliver_mouse_down xf yf state button zo id)

  and deliver_mouse_down xf yf state button zo id =
    let fig = IntMap.find id cc.model.fm in
    fig
      (MouseDown
         (xf, yf, state, button, cc.model, cc.cap, zo,
          (fun model' cap notifs ->
            cc.model <- model';
            cc.cap <- cap;
            List.iter notify notifs;
            invalidate ())))

  and win_cmd msg =
    match msg with
      Edit ->
       send nch (cch, DrawModel cc.model)
         (fun () ->
           recv cch always
             (fun msg ->
               match msg with
                 Cancel -> process ()
               | Update model ->
                  cc.model <- model;
                  invalidate ()
               | _ -> draw_error "draw edit"))
    | ResetOz ->
       cc.oz.scale <- 1.0; cc.oz.ox <- 0; cc.oz.oy <- 0; invalidate ()
    | AdjustOrigin ->
       if IntSet.is_empty cc.model.sel then
         process ()
       else
         adjust_origin cc.model.fm cc.model.sel cc.width cc.height cc.oz
           (fun (x, y) ->
             cc.oz.ox <- x;
             cc.oz.oy <- y;
             invalidate ())
    | AdjustOriginFor s ->
       if IntSet.is_empty s then
         process ()
       else
         adjust_origin cc.model.fm s cc.width cc.height cc.oz
           (fun (x, y) ->
             cc.oz.ox <- x;
             cc.oz.oy <- y;
             invalidate ())
    | RightMost ->
       right_most cc.model.fm cc.width cc.height cc.oz
         (fun (x, y) ->
           cc.oz.ox <- x;
           cc.oz.oy <- y;
           invalidate ())
    | PaintCommand (model, x, y) ->
       draw_figures model CapNone
         { ox=int_of_float x; oy=int_of_float y; scale=1.0; }
         (fun () ->
           send nch (cch, PaintCommandAck) process)

    | _ -> process ()

  and request msg =
    let e = sendEvt pch (wch, msg) pque_drop in
    Queue.add e pque;

  and notify msg =
    let e = sendEvt nch (cch, msg) nque_drop in
    Queue.add e nque

  and inv () =
    let msg = (wch, Invalidate (0, 0, cc.width, cc.height)) in
    let e = sendEvt pch msg clear_inv in
    cc.inv <- Some e

  and invalidate () = inv (); process ()
  and clear_inv () = cc.inv <- None; process ()
  and pque_drop () = let _ = Queue.take pque in process ()
  and nque_drop () = let _ = Queue.take nque in process ()

  in process ()

let new_zo_front fz =
  match IntMap.min_binding_opt fz with
    None -> 0
  | Some (zo, id) -> zo - 1

let new_zo_back fz =
  match IntMap.max_binding_opt fz with
    None -> 0
  | Some (zo, id) -> zo + 1

let new_id fm =
  match IntMap.max_binding_opt fm with
    None -> 0
  | Some (id, fig) -> id + 1

(*
  remove figure specified by id
  scanning fz is needed to find the entry for the id
*)
let remove_figure fz fm id k =
  IntMap.enum_lnr () fz
    (fun _ o next ->
      match o with
        None -> draw_error "remove_figure"
      | Some (zo, id') ->
         if id = id' then
           k (IntMap.remove zo fz) (IntMap.remove id fm)
         else
           next ())

let get_zo fz id k =
  IntMap.enum_lnr () fz
    (fun _ o next ->
      match o with
        None -> draw_error "get_zo"
      | Some (zo, id') ->
         if id = id' then
           k zo
         else
           next ())

let get_act_fig fm cap id =
  match cap with
    CapNone -> IntMap.find id fm
  | CapFigure (id', fig) ->
     if id' = id then fig else IntMap.find id fm
  | CapScroll _ -> IntMap.find id fm
  | CapSelect _ -> IntMap.find id fm
  | CapMove _ -> IntMap.find id fm

let offset_fig sel cap id x y =
  match cap with
    CapMove ((x0, y0), (x1, y1)) ->
     if IntSet.mem id sel then
       (x +. x1 -. x0, y +. y1 -. y0)
     else (x, y)
  | _ -> (x, y)
