open Graphics

(* Compilation : ocamlfind ocamlopt -package graphics -linkpkg io.ml main.ml -o simu *)

(*********)
(* Types *)
(*********)

type settings = { max_planes : int; radius : float }

type slider = {
  label : string;
  sl_x : int; sl_y : int; sl_w : int;
  vmin : int; vmax : int;
  mutable value : int;
}

(**************)
(* Constantes *)
(**************)
let win_w = 1000
let win_h = 700
let cx = 350                (* centre du secteur, en pixels *)
let cy = 350
let r_px = 320              (* rayon du secteur affiche, en pixels *)
let max_radius = 100        (* rayon max reglable, en NM *)

let grey = rgb 215 215 215

let open_window () =
  open_graph (Printf.sprintf " %dx%d" win_w win_h);
  set_window_title "Resolution de conflits - methodes reactives";
  auto_synchronize false

(************************)
(* Curseurs et bouton   *)
(************************)

let slider_px s = s.sl_x + (s.value - s.vmin) * s.sl_w / (s.vmax - s.vmin)

let draw_slider s =
  set_color black;
  moveto s.sl_x s.sl_y; lineto (s.sl_x + s.sl_w) s.sl_y;
  moveto s.sl_x (s.sl_y + 25);
  draw_string (Printf.sprintf "%s : %d" s.label s.value);
  set_color blue;
  fill_circle (slider_px s) s.sl_y 8;
  set_color black;
  draw_circle (slider_px s) s.sl_y 8

let on_slider s (x, y) =
  abs (y - s.sl_y) <= 14 && s.sl_x - 10 <= x && x <= s.sl_x + s.sl_w + 10

let set_slider s x =
  let x = max s.sl_x (min (s.sl_x + s.sl_w) x) in
  let range = s.vmax - s.vmin in
  s.value <- s.vmin + ((x - s.sl_x) * range + s.sl_w / 2) / s.sl_w

let btn_x = 700 and btn_y = 200 and btn_w = 250 and btn_h = 50

let on_button (x, y) =
  btn_x <= x && x <= btn_x + btn_w && btn_y <= y && y <= btn_y + btn_h

let draw_button () =
  set_color green; fill_rect btn_x btn_y btn_w btn_h;
  set_color black; draw_rect btn_x btn_y btn_w btn_h;
  moveto (btn_x + btn_w / 2 - 25) (btn_y + btn_h / 2 - 5);
  draw_string "Lancer"

(*********************)
(* Ecran de reglages *)
(*********************)

(* Apercu : uniquement le secteur a la taille choisie *)
let draw_preview (radius : int) (n : int) =
  let pr = r_px * radius / max_radius in
  set_color grey; fill_circle cx cy pr;
  set_color black; draw_circle cx cy pr;
  moveto (cx - 90) (cy + pr + 8);
  draw_string (Printf.sprintf "Secteur : rayon %d NM, %d avions max" radius n)

(* Retourne les reglages choisis *)
let settings_screen () : settings =
  let s_n = { label = "Nombre max d'avions"; sl_x = 700; sl_y = 500; sl_w = 250;
              vmin = 1; vmax = 50; value = 10 } in
  let s_r = { label = "Rayon du secteur (NM)"; sl_x = 700; sl_y = 400; sl_w = 250;
              vmin = 10; vmax = max_radius; value = 50 } in
  let dragging : slider option ref = ref None in
  let fin = ref false in
  let redraw () =
    clear_graph ();
    draw_preview s_r.value s_n.value;
    draw_slider s_n; draw_slider s_r;
    draw_button ();
    synchronize ()
  in
  redraw ();
  while not !fin do
    let st = wait_next_event [Button_down; Button_up; Mouse_motion] in
    let p = (st.mouse_x, st.mouse_y) in
    if st.button then begin
      match !dragging with
      | Some s -> set_slider s st.mouse_x
      | None ->
        if on_slider s_n p then (dragging := Some s_n; set_slider s_n st.mouse_x)
        else if on_slider s_r p then (dragging := Some s_r; set_slider s_r st.mouse_x)
        else if on_button p then fin := true
    end else dragging := None;
    redraw ()
  done;
  { max_planes = s_n.value; radius = float s_r.value }

(*********************************)
(* Affichage du secteur (vide)   *)
(*********************************)

let display_sector (s : settings) : unit =
  clear_graph ();
  set_color grey; fill_circle cx cy r_px;
  set_color black; draw_circle cx cy r_px;
  moveto 700 600; draw_string (Printf.sprintf "Rayon du secteur : %.0f NM" s.radius);
  moveto 700 570; draw_string (Printf.sprintf "Avions max : %d" s.max_planes);
  moveto 700 100; draw_string "Touche q : quitter";
  synchronize ()




(* true si l'utilisateur a appuye sur 'q' *)
let quit_requested () : bool =
  let q = ref false in
  while key_pressed () do
    if read_key () = 'q' then q := true
  done;
  !q


(*********************************)
(* Affichage avion               *)
(*********************************)