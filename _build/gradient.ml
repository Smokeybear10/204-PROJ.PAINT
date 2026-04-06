(** Gradient Lab — interactive color blending via inverse distance weighting *)
open Js_of_ocaml

(* ---- JS interop helpers ---- *)
let doc = Dom_html.document
let jss = Js.string
let inj = Js.Unsafe.inject
let call o m a = ignore (Js.Unsafe.meth_call o m a)
let get_f o p : float = Js.Unsafe.get o (jss p)
let set_p o p v = Js.Unsafe.set o (jss p) v
let el id = Js.Opt.get (doc##getElementById (jss id)) (fun () -> failwith id)
let get_str o p : string = Js.to_string (Js.Unsafe.get o (jss p))

(* ---- types ---- *)
type node = {
  mutable x: float; mutable y: float;
  r: int; g: int; b: int;
  mutable spread: float; mutable alpha: float;
}

type rgb = { cr: int; cg: int; cb: int }

(* ---- palette ---- *)
let palette = [|
  {cr=26;  cg=26;  cb=26};   (* ink    *)
  {cr=236; cg=104; cb=19};   (* orange *)
  {cr=45;  cg=110; cb=94};   (* teal   *)
  {cr=192; cg=57;  cb=43};   (* red    *)
  {cr=212; cg=160; cb=23};   (* gold   *)
  {cr=120; cg=81;  cb=169};  (* plum   *)
  {cr=86;  cg=160; cb=211};  (* sky    *)
  {cr=211; cg=94;  cb=131};  (* rose   *)
  {cr=56;  cg=124; cb=68};   (* forest *)
  {cr=235; cg=137; cb=100};  (* coral  *)
  {cr=108; cg=117; cb=125};  (* slate  *)
  {cr=245; cg=235; cb=210};  (* cream  *)
|]

(* ---- mutable state ---- *)
let nodes : node list ref = ref []
let history : node list list ref = ref []
let active_color = ref 1
let spread_v = ref 200.0
let alpha_v = ref 0.85
let blend_mode = ref "idw"
let bg_r = ref 255
let bg_g = ref 255
let bg_b = ref 255
let selected : node option ref = ref None
let dragging : node option ref = ref None

let save_history () =
  let snap = List.map (fun n ->
    { x=n.x; y=n.y; r=n.r; g=n.g; b=n.b;
      spread=n.spread; alpha=n.alpha }
  ) !nodes in
  history := snap :: !history;
  if List.length !history > 40 then
    history := List.filteri (fun i _ -> i < 40) !history

(* ---- canvas setup ---- *)
let canvas = el "c"
let ctx = Js.Unsafe.meth_call canvas "getContext" [| inj (jss "2d") |]
let cw = int_of_float (get_f canvas "width")
let ch = int_of_float (get_f canvas "height")

let sc = 2
let rw = cw / sc
let rh = ch / sc
let offscreen = Dom_html.createCanvas doc
let () = offscreen##.width := rw; offscreen##.height := rh
let octx = Js.Unsafe.meth_call offscreen "getContext" [| inj (jss "2d") |]
let pixels = Array.make (rw * rh * 4) 0.0

(* JS bridge: copy OCaml float array → ImageData → canvas *)
let blit_fn = Js.Unsafe.pure_js_expr
  "(function(a,c,w,h){\
     var i=c.createImageData(w,h),d=i.data;\
     for(var j=0;j<d.length;j++)d[j]=a[j+1];\
     c.putImageData(i,0,0)})"

let blit () =
  ignore (Js.Unsafe.fun_call blit_fn [|
    inj pixels; inj octx;
    inj (float_of_int rw); inj (float_of_int rh)
  |])

(* ---- IDW computation (pure OCaml) ---- *)
let compute () =
  let br = !bg_r and bgg = !bg_g and bb = !bg_b in
  let ns = !nodes in
  let bm = !blend_mode in
  for py = 0 to rh - 1 do
    for px = 0 to rw - 1 do
      let x = float_of_int (px * sc) in
      let y = float_of_int (py * sc) in
      let tw = ref 0.0 in
      let rs = ref 0.0 in
      let gs = ref 0.0 in
      let bs = ref 0.0 in
      List.iter (fun n ->
        let dx = x -. n.x and dy = y -. n.y in
        let d = sqrt (dx *. dx +. dy *. dy) in
        let t = min (d /. n.spread) 1.0 in
        let f = 1.0 -. t *. t in
        if f > 0.0 then begin
          let w = f *. f *. n.alpha in
          rs := !rs +. float_of_int n.r *. w;
          gs := !gs +. float_of_int n.g *. w;
          bs := !bs +. float_of_int n.b *. w;
          tw := !tw +. w
        end
      ) ns;
      let idx = (py * rw + px) * 4 in
      if !tw > 0.001 then begin
        if bm = "additive" then begin
          pixels.(idx)   <- min 255.0 (float_of_int br +. !rs);
          pixels.(idx+1) <- min 255.0 (float_of_int bgg +. !gs);
          pixels.(idx+2) <- min 255.0 (float_of_int bb +. !bs)
        end else begin
          let a = min !tw 1.0 in
          pixels.(idx)   <- float_of_int br *. (1.0 -. a) +. (!rs /. !tw) *. a;
          pixels.(idx+1) <- float_of_int bgg *. (1.0 -. a) +. (!gs /. !tw) *. a;
          pixels.(idx+2) <- float_of_int bb *. (1.0 -. a) +. (!bs /. !tw) *. a
        end
      end else begin
        pixels.(idx)   <- float_of_int br;
        pixels.(idx+1) <- float_of_int bgg;
        pixels.(idx+2) <- float_of_int bb
      end;
      pixels.(idx+3) <- 255.0
    done
  done

(* ---- render ---- *)
let render () =
  compute ();
  blit ();
  set_p ctx "imageSmoothingEnabled" Js._true;
  call ctx "drawImage" [|
    inj offscreen; inj 0.0; inj 0.0;
    inj (float_of_int cw); inj (float_of_int ch)
  |];
  (* tiny dot markers *)
  List.iter (fun n ->
    call ctx "beginPath" [||];
    call ctx "arc" [|
      inj n.x; inj n.y; inj 2.0; inj 0.0; inj (2.0 *. Float.pi)
    |];
    set_p ctx "fillStyle" (jss (
      match !selected with Some s when s == n -> "#ec6813"
      | _ -> "rgba(26,26,26,0.5)"
    ));
    call ctx "fill" [||]
  ) !nodes;
  (el "nodeVal")##.textContent := Js.some (jss (string_of_int (List.length !nodes)))

(* ---- hit testing ---- *)
let hit_test x y =
  let rec check = function
    | [] -> None
    | n :: rest ->
      match check rest with
      | Some _ as found -> found
      | None ->
        let dx = x -. n.x and dy = y -. n.y in
        if dx *. dx +. dy *. dy < 400.0 then Some n else None
  in check !nodes

(* ---- pointer coords ---- *)
let canvas_coords ev =
  let rect = Js.Unsafe.meth_call canvas "getBoundingClientRect" [||] in
  let cx = get_f ev "clientX" -. get_f rect "left" in
  let cy = get_f ev "clientY" -. get_f rect "top" in
  let rw = get_f rect "width" and rh = get_f rect "height" in
  (cx *. float_of_int cw /. rw, cy *. float_of_int ch /. rh)

(* ---- node operations ---- *)
let add_node x y =
  save_history ();
  let c = palette.(!active_color) in
  let n = { x; y; r=c.cr; g=c.cg; b=c.cb;
            spread= !spread_v; alpha= !alpha_v } in
  nodes := !nodes @ [n];
  selected := Some n;
  render ()

let remove_node n =
  save_history ();
  nodes := List.filter (fun nd -> nd != n) !nodes;
  (match !selected with Some s when s == n -> selected := None | _ -> ());
  render ()

let undo () =
  match !history with
  | [] -> ()
  | prev :: rest ->
    nodes := prev; history := rest; selected := None; render ()

(* ---- hex parsing for bg select ---- *)
let parse_hex s =
  let v = int_of_string ("0x" ^ String.sub s 1 6) in
  ((v lsr 16) land 255, (v lsr 8) land 255, v land 255)

(* ---- build color palette UI ---- *)
let build_palette () =
  let row = el "colors" in
  Array.iteri (fun i c ->
    let swatch = doc##createElement (jss "div") in
    swatch##.className := jss (
      if i = !active_color then "color-swatch active" else "color-swatch"
    );
    set_p (Js.Unsafe.get swatch (jss "style")) "background"
      (jss (Printf.sprintf "rgb(%d,%d,%d)" c.cr c.cg c.cb));
    swatch##.onclick := Dom_html.handler (fun _ ->
      active_color := i;
      let children = row##.childNodes in
      for j = 0 to children##.length - 1 do
        Js.Opt.iter (children##item j) (fun child ->
          Js.Opt.iter (Dom_html.CoerceTo.element child) (fun e ->
            e##.className := jss "color-swatch"
          ))
      done;
      swatch##.className := jss "color-swatch active";
      Js._true);
    Dom.appendChild row swatch
  ) palette

(* ---- initialization ---- *)
let () =
  Random.self_init ();
  build_palette ();

  (* mouse handlers *)
  set_p canvas "onmousedown" (Dom_html.handler (fun ev ->
    let (x, y) = canvas_coords ev in
    let btn = int_of_float (get_f ev "button") in
    if btn = 2 then
      (match hit_test x y with Some n -> remove_node n | None -> ())
    else
      (match hit_test x y with
       | Some n -> selected := Some n; dragging := Some n; render ()
       | None -> add_node x y);
    Js._true));

  set_p canvas "onmousemove" (Dom_html.handler (fun ev ->
    (match !dragging with
     | None -> ()
     | Some n ->
       let (x, y) = canvas_coords ev in
       n.x <- x; n.y <- y; render ());
    Js._true));

  set_p canvas "onmouseup" (Dom_html.handler (fun _ ->
    (match !dragging with
     | Some _ -> save_history (); dragging := None
     | None -> ());
    Js._true));

  set_p canvas "oncontextmenu" (Dom_html.handler (fun ev ->
    Dom.preventDefault ev; Js._false));

  (* sliders *)
  let spread_el = el "spread" in
  set_p spread_el "oninput" (Dom_html.handler (fun _ ->
    spread_v := float_of_string (get_str spread_el "value");
    (el "spreadVal")##.textContent :=
      Js.some (jss (string_of_int (int_of_float !spread_v)));
    (match !selected with Some n -> n.spread <- !spread_v; render () | None -> ());
    Js._true));

  let alpha_el = el "alpha" in
  set_p alpha_el "oninput" (Dom_html.handler (fun _ ->
    alpha_v := float_of_string (get_str alpha_el "value") /. 100.0;
    (el "alphaVal")##.textContent :=
      Js.some (jss (Printf.sprintf "%.2f" !alpha_v));
    (match !selected with Some n -> n.alpha <- !alpha_v; render () | None -> ());
    Js._true));

  let blend_el = el "blend" in
  set_p blend_el "onchange" (Dom_html.handler (fun _ ->
    blend_mode := get_str blend_el "value";
    render (); Js._true));

  let bg_el = el "bg" in
  set_p bg_el "onchange" (Dom_html.handler (fun _ ->
    let hex = get_str bg_el "value" in
    let (r, g, b) = parse_hex hex in
    bg_r := r; bg_g := g; bg_b := b;
    render (); Js._true));

  (* buttons *)
  set_p (el "clearBtn") "onclick" (Dom_html.handler (fun _ ->
    save_history (); nodes := []; selected := None; render (); Js._true));

  set_p (el "undoBtn") "onclick" (Dom_html.handler (fun _ ->
    undo (); Js._true));

  set_p (el "randBtn") "onclick" (Dom_html.handler (fun _ ->
    save_history (); nodes := [];
    let count = 4 + Random.int 5 in
    for _ = 1 to count do
      let c = palette.(Random.int (Array.length palette)) in
      nodes := !nodes @ [{
        x = 60.0 +. Random.float (float_of_int cw -. 120.0);
        y = 60.0 +. Random.float (float_of_int ch -. 120.0);
        r=c.cr; g=c.cg; b=c.cb;
        spread = 120.0 +. Random.float 280.0;
        alpha = 0.5 +. Random.float 0.5;
      }]
    done;
    selected := None; render (); Js._true));

  set_p (el "exportBtn") "onclick" (Dom_html.handler (fun _ ->
    let prev = !selected in
    selected := None;
    compute (); blit ();
    set_p ctx "imageSmoothingEnabled" Js._true;
    call ctx "drawImage" [|
      inj offscreen; inj 0.0; inj 0.0;
      inj (float_of_int cw); inj (float_of_int ch)
    |];
    let url = Js.Unsafe.meth_call canvas "toDataURL" [| inj (jss "image/png") |] in
    let link = Dom_html.createA doc in
    set_p link "download" (jss "gradient.png");
    link##.href := url;
    link##click;
    selected := prev; render ();
    Js._true));

  (* keyboard *)
  set_p doc "onkeydown" (Dom_html.handler (fun ev ->
    let key = Js.to_string (Js.Unsafe.get ev (jss "key")) in
    if key = "z" || key = "Z" then undo ()
    else if key = "Delete" || key = "Backspace" then
      (match !selected with Some n -> remove_node n | None -> ());
    Js._true));

  render ()
