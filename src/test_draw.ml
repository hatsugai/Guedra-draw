open Csp
open Guedra
open DrawModel

let make_figs n fz fm =
  let rec loop k pid fz fm =
    if k = n then
      (fz, fm)
    else
      let qzo = Draw.new_zo_front fz in
      let qid = Draw.new_id fm in
      let qfig = FigRect.make qid 10.0 150.0 100.0 50.0 in
      let fz = IntMap.add qzo qid fz in
      let fm = IntMap.add qid qfig fm in
      let czo = Draw.new_zo_back fz in
      let cid = Draw.new_id fm in
      let cfig = FigConnLabel.make pid qid
                   (Printf.sprintf "label %d" k) 0.0 0.0 in
      (* let cfig = FigConn.make pid qid in *)
      let fz = IntMap.add czo cid fz in
      let fm = IntMap.add cid cfig fm in
      loop (k+1) qid fz fm
  in
  let pzo = Draw.new_zo_front fz in
  let pid = Draw.new_id fm in
  let pfig = FigRect.make pid 10.0 10.0 100.0 50.0 in
  let fz = IntMap.add pzo pid fz in
  let fm = IntMap.add pid pfig fm in
  loop 0 pid fz fm

let rec main cch nch =
  let rec process () =
    recv nch always
      (fun (cch, msg) -> process ())
  in
  (* init: make and add figures *)
  send cch Edit
    (fun () ->
      recv nch always
        (fun (cch, msg) ->
          match msg with
            DrawModel model ->
             let (fz, fm) = make_figs 10 model.fz model.fm in
             let model' = { model with fz=fz; fm=fm; } in
             send cch (Update model') process
          | _ ->
             process () ))

let init () =
  let (wch, pch) = create_toplevel_window "test window" 0 0 1024 600 in
  let cch = make_chan () in
  let nch = make_chan () in
  par [
      (fun () -> Draw.init wch pch cch nch false);
      (fun () -> main cch nch)]

let () = reg_client init
