open Io
(*dt base 1 sec*)
let dt = 1.
let steps_per_frame = 5      (* acceleration : 5 s simulees par image *)
let lambda = 1. /. 20.       (* arrivees par seconde : 1 avion / 20 s en moyenne *)
 
let () =
  Random.self_init ();
  open_window ();
  (* 1. ecran de configuration : nombre max d'avions et rayon du secteur *)
  let s = settings_screen () in
  (* 2. trafic *)
  let t = Traffic.create_traffic s.max_planes s.radius lambda in
  (* 3. boucle de simulation, 'q' pour quitter *)
  let fin = ref false and temps = ref 0. in
  while not !fin do
    for _ = 1 to steps_per_frame do
      Traffic.step_traffic t dt;
      temps := !temps +. dt
    done;
    display_traffic s t.Traffic.avions t.Traffic.nb_avions !temps;
    if quit_requested () then fin := true;
    Unix.sleepf 0.03
  done;
  Graphics.close_graph ()
 