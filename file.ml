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

let rec get_enfants arbre =
  match arbre with
  | Noeud(enfants) -> List.filter (fun el -> el <> Feuille) enfants (* on enlève les feuilles *)
  | Feuille -> [] 

(* fonction très utile pour voir si l'arbre binomial à bien la bonne forme *)
let afficher_nb_noeud_par_niveau arbre_binomial =
  let racine = [arbre_binomial] in
  let rec aux liste niveau =
    let nb_noeud = List.length liste in
    Printf.printf "Niveau : %d , Nombre noeud : %d\n" niveau nb_noeud ;
    let enfants = List.flatten (List.map (fun el -> get_enfants el) liste) in
    if enfants <> [] then aux enfants (niveau+1)
  in aux racine 0

                 

             
let exemple_arbre = creer_arbre_binomial 3;;
afficher_nb_noeud_par_niveau exemple_arbre;;
