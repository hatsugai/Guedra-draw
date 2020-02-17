exception Draw_error of string

let draw_error s = raise (Draw_error s)

module IntSet =
  Setx.Make(
      struct
        type t = int
        let compare x y = x - y
      end)

module IntMap =
  struct
    module X =
      Mapx.Make
        (struct
          type t = int
          let compare = compare
        end)
    include X

    let find key map =
      match find_opt key map with
        Some v -> v
      | None ->
         draw_error (Printf.sprintf "IntMap: unknown key: %d" key)

    let find_with_default key map default =
      match find_opt key map with
        Some v -> v
      | None -> default
  end

type oz = {
    mutable ox : int;
    mutable oy : int;
    mutable scale : float;
  }

(*
  'a: ext
  'b: prop
  'c: connect type
*)

type ('a, 'b, 'c) figure = ('a, 'b, 'c) op -> unit

and ('a, 'b, 'c) model = {
    fz : int IntMap.t;
    fm : ('a, 'b, 'c) figure IntMap.t;
    sel : IntSet.t;
    ext : 'a
  }

and ('a, 'b, 'c) op =
  HitTest of float * float * (bool -> unit)
| Draw of ('a, 'b, 'c) model
          * ('a, 'b, 'c) capture         (* cap *)
          * oz
          * (unit -> unit)
| MouseDown of float * float                      (* x, y *)
               * int * int                        (* state, button *)
               * ('a, 'b, 'c) model
               * ('a, 'b, 'c) capture
               * int            (* zo *)
               * (('a, 'b, 'c) model
                  -> ('a, 'b, 'c) capture
                  -> ('a, 'b, 'c) notification list
                  -> unit)
| MouseMove of float * float * int
               * (('a, 'b, 'c) figure -> unit)
| MouseUp of float * float * int * int
             * (('a, 'b, 'c) figure -> unit)
| ConnectPoint of 'c * ('c -> float -> float -> unit)
| Boundary of (float -> float -> float -> float -> unit)
| GetProp of ('b -> unit)
| SetProp of 'b * ('b -> ('a, 'b, 'c) figure -> unit)
| Move of float * float * (('a, 'b, 'c) figure -> unit)
| Resize of float * float * (('a, 'b, 'c) figure -> unit)

and ('a, 'b, 'c) command =
| Edit
| Update of ('a, 'b, 'c) model
| Cancel
| ResetOz
| AdjustOrigin
| AdjustOriginFor of IntSet.t
| RightMost
| PaintCommand of ('a, 'b, 'c) model * float * float (* offset *)

and ('a, 'b, 'c) notification =
  OnSelected of ('a, 'b, 'c) model
| OnChanged of ('a, 'b, 'c) model * ('a, 'b, 'c) model
| OnKeyDown of (int * int)
| DrawModel of ('a, 'b, 'c) model
| OnScroll of ('a, 'b, 'c) model * int * int * float * float
| PaintCommandAck

and ('a, 'b, 'c) capture =
  CapNone
| CapFigure of (int * ('a, 'b, 'c) figure)
| CapScroll of int * int * int * int
| CapSelect of (float * float) * (float * float)
               * IntSet.t       (* tsel *)
| CapMove of ((float * float) * (float * float))

