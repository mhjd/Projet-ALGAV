type arbre_binomial =
  | Noeud of enfants 
  | Feuille
and enfants = arbre_binomial list

let concat_le_double_arbre arbre_binomial =
  match arbre_binomial with 
  | Feuille -> failwith "Erreur : c'est une feuille, pas un arbre"
  | Noeud(liste_enfants) -> Noeud(arbre_binomial::liste_enfants)
  
let rec creer_arbre_binomial (taille:int):arbre_binomial =
  match taille with
  | 0 -> Noeud([Feuille; Feuille])
  | n -> concat_le_double_arbre (creer_arbre_binomial (n-1)) 

let afficher_nb_noeud_par_niveau arbre_binomial =
  (* alterner entre deux liste, dans lesquels on met tout les enfant d'un niveau, puis on fait l'inverse, etc *)

             
let exemple_arbre = creer_arbre_binomial 4;;
afficher_arbre_binomial exemple_arbre;;
