(** Tile Maker — pixel grid editor with live tessellation preview *)
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

(* ---- palette ---- *)
let colors = [|
  "#f5f0e6"; (* paper  *)
  "#1a1a1a"; (* ink    *)
  "#ec6813"; (* orange *)
  "#2d6e5e"; (* teal   *)
  "#c0392b"; (* red    *)
  "#d4a017"; (* gold   *)
  "#7851a9"; (* plum   *)
  "#56a0d3"; (* sky    *)
  "#d35e83"; (* rose   *)
  "#387c44"; (* forest *)
  "#eb8964"; (* coral  *)
  "#6c757d"; (* slate  *)
|]

let color_names = [|
  "paper"; "ink"; "orange"; "teal"; "red"; "gold";
  "plum"; "sky"; "rose"; "forest"; "coral"; "slate"
|]

(* ---- mutable state ---- *)
let tile_size = ref 16
let grid : int array ref = ref (Array.make (16 * 16) 0)
let history : int array list ref = ref []
let active_color = ref 2 (* orange *)
let tool = ref "draw"
let symmetry = ref "none"
let painting = ref false
let line_start : (int * int) option ref = ref None

let editor_px = 320

let save_history () =
  history := Array.copy !grid :: !history;
  if List.length !history > 50 then
    history := List.filteri (fun i _ -> i < 50) !history

(* ---- canvas setup ---- *)
let editor = el "editor"
let ectx = Js.Unsafe.meth_call editor "getContext" [| inj (jss "2d") |]
let preview = el "preview"
let pctx = Js.Unsafe.meth_call preview "getContext" [| inj (jss "2d") |]

(* offscreen tile buffer *)
let tile_buf = Dom_html.createCanvas doc
let tctx = Js.Unsafe.meth_call tile_buf "getContext" [| inj (jss "2d") |]

let resize () =
  let ts = !tile_size in
  set_p editor "width" (float_of_int editor_px);
  set_p editor "height" (float_of_int editor_px);
  set_p (Js.Unsafe.get editor (jss "style")) "width" (jss (string_of_int editor_px ^ "px"));
  set_p (Js.Unsafe.get editor (jss "style")) "height" (jss (string_of_int editor_px ^ "px"));
  let wrap = el "canvasWrap" in
  let pw = int_of_float (get_f wrap "clientWidth") in
  let ph = int_of_float (get_f wrap "clientHeight") in
  set_p preview "width" (float_of_int pw);
  set_p preview "height" (float_of_int ph);
  set_p (Js.Unsafe.get preview (jss "style")) "width" (jss (string_of_int pw ^ "px"));
  set_p (Js.Unsafe.get preview (jss "style")) "height" (jss (string_of_int ph ^ "px"));
  tile_buf##.width := ts;
  tile_buf##.height := ts

(* ---- algorithms (pure OCaml) ---- *)
let set_pixel x y ci =
  let ts = !tile_size in
  if x >= 0 && x < ts && y >= 0 && y < ts then
    !grid.(y * ts + x) <- ci

let apply_symmetry x y ci =
  let ts = !tile_size in
  set_pixel x y ci;
  let sym = !symmetry in
  if sym = "h" || sym = "4way" then set_pixel (ts - 1 - x) y ci;
  if sym = "v" || sym = "4way" then set_pixel x (ts - 1 - y) ci;
  if sym = "4way" then set_pixel (ts - 1 - x) (ts - 1 - y) ci

let flood_fill sx sy new_ci =
  let ts = !tile_size in
  let g = !grid in
  let old_ci = g.(sy * ts + sx) in
  if old_ci = new_ci then ()
  else begin
    let stack = ref [(sx, sy)] in
    let visited = Array.make (ts * ts) false in
    while !stack <> [] do
      match !stack with
      | [] -> ()
      | (x, y) :: rest ->
        stack := rest;
        if x >= 0 && x < ts && y >= 0 && y < ts then begin
          let idx = y * ts + x in
          if not visited.(idx) && g.(idx) = old_ci then begin
            visited.(idx) <- true;
            g.(idx) <- new_ci;
            stack := (x-1,y) :: (x+1,y) :: (x,y-1) :: (x,y+1) :: !stack
          end
        end
    done
  end

let draw_line x0 y0 x1 y1 ci =
  let dx = abs (x1 - x0) and dy = abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 in
  let sy = if y0 < y1 then 1 else -1 in
  let err = ref (dx - dy) in
  let cx = ref x0 and cy = ref y0 in
  let stop = ref false in
  while not !stop do
    apply_symmetry !cx !cy ci;
    if !cx = x1 && !cy = y1 then stop := true
    else begin
      let e2 = 2 * !err in
      if e2 > -dy then begin err := !err - dy; cx := !cx + sx end;
      if e2 < dx then begin err := !err + dx; cy := !cy + sy end
    end
  done

(* ---- rendering ---- *)
let render_tile_buf () =
  let ts = !tile_size in
  let g = !grid in
  for y = 0 to ts - 1 do
    for x = 0 to ts - 1 do
      let ci = g.(y * ts + x) in
      set_p tctx "fillStyle" (jss colors.(ci));
      call tctx "fillRect" [|
        inj (float_of_int x); inj (float_of_int y);
        inj 1.0; inj 1.0
      |]
    done
  done

let render_preview () =
  render_tile_buf ();
  set_p pctx "imageSmoothingEnabled" Js._false;
  let scale = max 4 (editor_px / !tile_size) in
  let tw = float_of_int (!tile_size * scale) in
  let th = tw in
  let pw = get_f preview "width" in
  let ph = get_f preview "height" in
  let y = ref (-.th) in
  while !y < ph +. th do
    let x = ref (-.tw) in
    while !x < pw +. tw do
      call pctx "drawImage" [|
        inj tile_buf; inj !x; inj !y; inj tw; inj th
      |];
      x := !x +. tw
    done;
    y := !y +. th
  done

let render_editor () =
  let ts = !tile_size in
  let cpx = float_of_int editor_px /. float_of_int ts in
  let g = !grid in
  set_p ectx "imageSmoothingEnabled" Js._false;
  for y = 0 to ts - 1 do
    for x = 0 to ts - 1 do
      let ci = g.(y * ts + x) in
      set_p ectx "fillStyle" (jss colors.(ci));
      call ectx "fillRect" [|
        inj (float_of_int x *. cpx); inj (float_of_int y *. cpx);
        inj cpx; inj cpx
      |]
    done
  done;
  (* grid lines *)
  set_p ectx "strokeStyle" (jss "rgba(26,26,26,0.12)");
  set_p ectx "lineWidth" (0.5 : float);
  for i = 0 to ts do
    let pos = float_of_int i *. cpx in
    call ectx "beginPath" [||];
    call ectx "moveTo" [| inj pos; inj 0.0 |];
    call ectx "lineTo" [| inj pos; inj (float_of_int editor_px) |];
    call ectx "moveTo" [| inj 0.0; inj pos |];
    call ectx "lineTo" [| inj (float_of_int editor_px); inj pos |];
    call ectx "stroke" [||]
  done

let render () =
  render_preview ();
  render_editor ()

(* ---- cell from pointer ---- *)
let cell_at ev =
  let rect = Js.Unsafe.meth_call editor "getBoundingClientRect" [||] in
  let ts = !tile_size in
  let rw = get_f rect "width" in
  let rh = get_f rect "height" in
  let mx = get_f ev "clientX" -. get_f rect "left" in
  let my = get_f ev "clientY" -. get_f rect "top" in
  let x = int_of_float (mx /. rw *. float_of_int ts) in
  let y = int_of_float (my /. rh *. float_of_int ts) in
  (max 0 (min (ts-1) x), max 0 (min (ts-1) y))

(* ---- undo ---- *)
let undo () =
  match !history with
  | [] -> ()
  | prev :: rest ->
    if Array.length prev = Array.length !grid then
      grid := prev;
    history := rest;
    render ()

(* ---- build palette UI ---- *)
let build_palette () =
  let row = el "colors" in
  row##.innerHTML := jss "";
  Array.iteri (fun i hex ->
    let swatch = doc##createElement (jss "div") in
    swatch##.className := jss (
      if i = !active_color then "color-swatch active" else "color-swatch"
    );
    set_p (Js.Unsafe.get swatch (jss "style")) "background" (jss hex);
    set_p swatch "title" (jss color_names.(i));
    swatch##.onclick := Dom_html.handler (fun _ ->
      active_color := i;
      let children = row##.childNodes in
      for j = 0 to children##.length - 1 do
        Js.Opt.iter (children##item j) (fun child ->
          Js.Opt.iter (Dom_html.CoerceTo.element child) (fun e ->
            e##.className := jss "color-swatch"))
      done;
      swatch##.className := jss "color-swatch active";
      Js._true);
    Dom.appendChild row swatch
  ) colors

(* ---- init demo pattern ---- *)
let init_demo () =
  let ts = !tile_size in
  let mid = ts / 2 in
  for i = 0 to ts - 1 do
    !grid.(i * ts + mid) <- 2;  (* orange vertical *)
    !grid.(mid * ts + i) <- 2   (* orange horizontal *)
  done;
  !grid.(2 * ts + 2) <- 3;               (* teal *)
  !grid.(2 * ts + (ts - 3)) <- 4;        (* red *)
  !grid.((ts - 3) * ts + 2) <- 5;        (* gold *)
  !grid.((ts - 3) * ts + (ts - 3)) <- 6; (* plum *)
  !grid.(mid * ts + mid) <- 1            (* ink center *)

(* ---- initialization ---- *)
let () =
  Random.self_init ();
  build_palette ();
  resize ();
  init_demo ();

  (* tool buttons *)
  let tool_btns = doc##querySelectorAll (jss ".tool-btn") in
  for i = 0 to tool_btns##.length - 1 do
    Js.Opt.iter (tool_btns##item i) (fun btn ->
      set_p btn "onclick" (Dom_html.handler (fun _ ->
        tool := get_str (Js.Unsafe.get btn (jss "dataset")) "tool";
        for j = 0 to tool_btns##.length - 1 do
          Js.Opt.iter (tool_btns##item j) (fun b ->
            Js.Opt.iter (Dom_html.CoerceTo.element b) (fun e ->
              e##.className := jss "tool-btn"))
        done;
        Js.Opt.iter (Dom_html.CoerceTo.element btn) (fun e ->
          e##.className := jss "tool-btn active");
        Js._true)))
  done;

  (* symmetry buttons *)
  let sym_btns = doc##querySelectorAll (jss ".sym-btn") in
  for i = 0 to sym_btns##.length - 1 do
    Js.Opt.iter (sym_btns##item i) (fun btn ->
      set_p btn "onclick" (Dom_html.handler (fun _ ->
        symmetry := get_str (Js.Unsafe.get btn (jss "dataset")) "sym";
        for j = 0 to sym_btns##.length - 1 do
          Js.Opt.iter (sym_btns##item j) (fun b ->
            Js.Opt.iter (Dom_html.CoerceTo.element b) (fun e ->
              e##.className := jss "sym-btn"))
        done;
        Js.Opt.iter (Dom_html.CoerceTo.element btn) (fun e ->
          e##.className := jss "sym-btn active");
        Js._true)))
  done;

  (* tile size *)
  let size_el = el "tileSize" in
  set_p size_el "onchange" (Dom_html.handler (fun _ ->
    save_history ();
    let new_size = int_of_string (get_str size_el "value") in
    let new_grid = Array.make (new_size * new_size) 0 in
    let min_s = min !tile_size new_size in
    for y = 0 to min_s - 1 do
      for x = 0 to min_s - 1 do
        new_grid.(y * new_size + x) <- !grid.(y * !tile_size + x)
      done
    done;
    tile_size := new_size;
    grid := new_grid;
    resize (); render ();
    Js._true));

  (* editor pointer events *)
  set_p editor "onpointerdown" (Dom_html.handler (fun ev ->
    let (x, y) = cell_at ev in
    ignore (Js.Unsafe.meth_call editor "setPointerCapture"
      [| Js.Unsafe.get ev (jss "pointerId") |]);
    painting := true;
    if !tool = "fill" then begin
      save_history (); flood_fill x y !active_color; render ()
    end else if !tool = "line" then
      line_start := Some (x, y)
    else begin
      save_history ();
      let ci = if !tool = "erase" then 0 else !active_color in
      apply_symmetry x y ci; render ()
    end;
    Js._true));

  set_p editor "onpointermove" (Dom_html.handler (fun ev ->
    if !painting then begin
      let (x, y) = cell_at ev in
      if !tool = "draw" || !tool = "erase" then begin
        let ci = if !tool = "erase" then 0 else !active_color in
        apply_symmetry x y ci; render ()
      end
    end;
    Js._true));

  set_p editor "onpointerup" (Dom_html.handler (fun ev ->
    if !painting then begin
      painting := false;
      if !tool = "line" then begin
        match !line_start with
        | Some (sx, sy) ->
          let (ex, ey) = cell_at ev in
          save_history ();
          draw_line sx sy ex ey !active_color;
          line_start := None; render ()
        | None -> ()
      end
    end;
    Js._true));

  (* action buttons *)
  set_p (el "clearBtn") "onclick" (Dom_html.handler (fun _ ->
    save_history (); Array.fill !grid 0 (Array.length !grid) 0;
    render (); Js._true));

  set_p (el "undoBtn") "onclick" (Dom_html.handler (fun _ ->
    undo (); Js._true));

  set_p (el "randBtn") "onclick" (Dom_html.handler (fun _ ->
    save_history ();
    let ts = !tile_size in
    let used = Array.init 4 (fun _ -> Random.int (Array.length colors)) in
    for i = 0 to Array.length !grid - 1 do
      !grid.(i) <- if Random.float 1.0 < 0.35
        then used.(Random.int 4) else 0
    done;
    (* apply symmetry to random pattern *)
    let sym = !symmetry in
    if sym = "h" || sym = "4way" then
      for y = 0 to ts-1 do
        for x = 0 to ts/2 - 1 do
          !grid.(y * ts + (ts-1-x)) <- !grid.(y * ts + x)
        done
      done;
    if sym = "v" || sym = "4way" then
      for y = 0 to ts/2 - 1 do
        for x = 0 to ts-1 do
          !grid.((ts-1-y) * ts + x) <- !grid.(y * ts + x)
        done
      done;
    render (); Js._true));

  set_p (el "exportBtn") "onclick" (Dom_html.handler (fun _ ->
    let ts = !tile_size in
    render_tile_buf ();
    let exp = Dom_html.createCanvas doc in
    let ew = ts * 8 in
    exp##.width := ew; exp##.height := ew;
    let xctx = Js.Unsafe.meth_call exp "getContext" [| inj (jss "2d") |] in
    set_p xctx "imageSmoothingEnabled" Js._false;
    let tw = float_of_int (ew / 4) in
    for y = 0 to 3 do
      for x = 0 to 3 do
        call xctx "drawImage" [|
          inj tile_buf;
          inj (float_of_int x *. tw); inj (float_of_int y *. tw);
          inj tw; inj tw
        |]
      done
    done;
    let url = Js.Unsafe.meth_call exp "toDataURL" [| inj (jss "image/png") |] in
    let link = Dom_html.createA doc in
    set_p link "download" (jss "tile.png");
    link##.href := url;
    link##click;
    Js._true));

  (* keyboard *)
  set_p doc "onkeydown" (Dom_html.handler (fun ev ->
    let key = Js.to_string (Js.Unsafe.get ev (jss "key")) in
    if key = "z" || key = "Z" then undo ()
    else if key = "c" || key = "C" then begin
      save_history (); Array.fill !grid 0 (Array.length !grid) 0; render ()
    end
    else if key = "r" || key = "R" then
      ignore (Js.Unsafe.meth_call (el "randBtn") "click" [||])
    else if key = "1" then
      ignore (Js.Unsafe.meth_call
        (doc##querySelector (jss "[data-tool='draw']")) "click" [||])
    else if key = "2" then
      ignore (Js.Unsafe.meth_call
        (doc##querySelector (jss "[data-tool='erase']")) "click" [||])
    else if key = "3" then
      ignore (Js.Unsafe.meth_call
        (doc##querySelector (jss "[data-tool='fill']")) "click" [||])
    else if key = "4" then
      ignore (Js.Unsafe.meth_call
        (doc##querySelector (jss "[data-tool='line']")) "click" [||]);
    Js._true));

  (* window resize *)
  set_p (Dom_html.window) "onresize" (Dom_html.handler (fun _ ->
    resize (); render (); Js._true));

  render ()
