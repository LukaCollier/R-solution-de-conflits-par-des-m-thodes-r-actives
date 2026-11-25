(*********************************)
(*          vecteur              *)
(*********************************)

type vec = { x : float; y : float }

let pi = 4. *. atan 1.

let ( +| ) a b = { x = a.x +. b.x; y = a.y +. b.y }
let ( -| ) a b = { x = a.x -. b.x; y = a.y -. b.y }
let ( *| ) k a = { x = k *. a.x; y = k *. a.y }
let dot a b = a.x *. b.x +. a.y *. b.y
let norme a = sqrt (dot a a)
let dist a b = norme (a -| b)
let polaire r t = { x = r *. cos t; y = r *. sin t }
let angle a = atan2 a.y a.x

(*********************************)
(*            avion              *)
(*********************************)

(*speed en noeuds, cap en radians, target en coordonnées cartésiennes, pos en coordonnées cartésiennes *)
type avion ={mutable id:int; mutable speed:float; mutable cap: float; mutable target: vec; mutable pos: vec}

let avion_vide = {id= -1; speed=0.; cap=0.; target={x=0.; y=0.}; pos={x=0.; y=0.}}

let separation = 5.                  (* norme de separation horizontale (NM) *)
let taux_virage = 3. *. pi /. 180.   (* virage au taux standard : 3 deg/s *)
let kt = 1. /. 3600.                 (* 1 noeud en NM/s *)
let tolerance_sortie = 1.            (* sortie atteinte a moins de 1 NM *)
let marge_secteur = 2.               (* au dela de rayon + marge, l'avion a quitte le secteur *)


(*normalisation angle*)
let normalise t =
  let u = mod_float (t +. pi) (2. *. pi) in
    (if u < 0. then u +. 2. *. pi else u) -. pi
(*generate*)

let generate_avion id speed cap target pos = {id; speed; cap; target; pos}

(**** mouvement/ update avion ****)

(*update vitesse*)
let update_speed avion new_speed = avion.speed <- new_speed

(*turn*)
let turn avion dt new_cap =
  let delta = normalise (new_cap -. avion.cap) in
  let delta_max = taux_virage *. dt in
  if delta > delta_max then avion.cap <- avion.cap +. delta_max
  else if delta < -.delta_max then avion.cap <- avion.cap -. delta_max
  else avion.cap <- new_cap

(*advance*)

let advance avion dt =
  let nr = kt*.avion.speed *. dt in 
  let dvec = polaire nr avion.cap in
  avion.pos <- avion.pos +| dvec
