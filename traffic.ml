open Avions
(*type traffic*)
(*avions = tableau d'avions, max_avions = nombre maximum d'avions, rayon = rayon du secteur (NM)*)
(*lambda = taux pour la génération des avions (processus de Poisson)*)
(*nb_avions = nombre actuel d'avions dans le secteur*)
(*id_counter = compteur pour attribuer des identifiants uniques aux avions*)

type traffic = {
  mutable avions : avion array;
  max_avions : int;
  rayon : int;
  lambda : float;
  mutable nb_avions : int; 
  mutable id_counter : int;
}

(* Création d'une instance de traffic *)
(* id = -1  équivalent à un avion non initialisé *)
let create_traffic max_avions rayon lambda = {
  avions = Array.make max_avions {id=-1; speed=0.; cap=0.; target={x=0.; y=0.}; pos={x=0.; y=0.}};
  max_avions;
  rayon;
  lambda;
  nb_avions = 0;
  id_counter = 0;
}

(*génération du trafic suivant le processus de Poisson*)


(*Algorithme d'évitement de collision*)
(* liste d'algorithmes a developper *)




(*boucle de simulation*)

