open Io
(*dt base 1 sec*)
let () =
  open_window ();
  (* 1. ecran de configuration : nombre max d'avions et rayon du secteur *)
  let s = settings_screen () in
  (* 2. affichage du secteur circulaire *)
  display_sector s;
  (* 3. on attend 'q' pour quitter *)
  let fin = ref false in
  while not !fin do
    let st = Graphics.wait_next_event [Graphics.Key_pressed] in
    if st.Graphics.key = 'q' then fin := true
  done;
  Graphics.close_graph ()
