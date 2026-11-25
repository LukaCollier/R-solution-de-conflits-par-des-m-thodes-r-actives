open Avions
(*type traffic*)
(*avions = tableau d'avions, max_avions = nombre maximum d'avions, rayon = rayon du secteur (NM)*)
(*lambda = taux pour la génération des avions (processus de Poisson)*)
(*nb_avions = nombre actuel d'avions dans le secteur*)
(*id_counter = compteur pour attribuer des identifiants uniques aux avions*)

type traffic = {
  mutable avions : avion array;
  max_avions : int;
  rayon : float;
  lambda : float;
  mutable nb_avions : int; 
  mutable id_counter : int;
}

(* Création d'une instance de traffic *)
(* id = -1  équivalent à un avion non initialisé *)
let create_traffic max_avions rayon lambda = {
  avions = Array.init max_avions (fun _ -> avion_vide);
  max_avions;
  rayon;
  lambda;
  nb_avions = 0;
  id_counter = 0;
}

(*génération du trafic suivant le processus de Poisson*)

let knuth_poisson lambda = (* renvoie le nombre de naissances dans un intervalle de temps donné *)
  let l = exp (-.lambda) in
  let rec loop k p =
    let u = Random.float 1. in (*variable aléatoire uniforme U(0,1)*)
    let p' = p *. u in
    if p' <= l then k
    else loop (k + 1) p'
  in
  loop 0 1.

let emplacement_libre (t : traffic) =
  let rec aux i =
    if i >= t.max_avions then None
    else if t.avions.(i).id = -1 then Some i
    else aux (i + 1)
  in
  aux 0

let generate_plane (t : traffic) =
  match emplacement_libre t with
  | None -> ()  (* Pas d'emplacement libre *)
  | Some index ->
      let id = t.id_counter in
      let speed = Random.float 200. +. 100. in 
      let cap = Random.float (2. *. pi) -. pi in       
      let angleinit = Random.float (2. *. pi) in
      let anglefinal = Random.float (2. *. pi) in
      let pos = polaire t.rayon angleinit in
      let target = polaire t.rayon anglefinal in
      let new_plane = { id; speed; cap; target; pos } in
      t.avions.(index) <- new_plane;
      t.nb_avions <- t.nb_avions + 1;
      t.id_counter <- t.id_counter + 1


let generate_traffic (t :traffic) dt = (*génération du trafic suivant le processus de Poisson*)
  let n = knuth_poisson (t.lambda *. dt) in
  for _ = 1 to n do
    generate_plane t
  done
(*algorithme détection de conflits*)

(*Algorithme d'évitement de collision*)
(* liste d'algorithmes a developper *)




(*boucle de simulation*)

