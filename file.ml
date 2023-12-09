open Cle 

type degre = int ;;

type tournoi_binomial =
  | Noeud of degre * cle * enfants 
  | Feuille of cle
  | Vide
and enfants = tournoi_binomial list

type file_binomial = tournoi_binomial list

let rec get_enfants (tb:tournoi_binomial):(tournoi_binomial list) =
  match tb with
  | Noeud(_, _, enfants) -> enfants 
  | Feuille(_) -> [] 
  | Vide -> []

(* fonction très utile pour voir si l'arbre binomial à bien la bonne forme *)
let afficher_nb_noeud_par_niveau tb =
  let racine = [tb] in
  let rec aux liste niveau =
    let nb_noeud = List.length liste in
    Printf.printf "Niveau : %d , Nombre noeud : %d\n" niveau nb_noeud ;
    let enfants = List.flatten (List.map (fun el -> get_enfants el) liste) in
    if enfants <> [] then aux enfants (niveau+1)
  in aux racine 0

(* opérations sur arbre binomial, on commence par ça car plus simple *)

let zero = (Int32.of_int 0,Int32.of_int 0, Int32.of_int 0, Int32.of_int 0) ;;

let concat_le_double_arbre (tb:tournoi_binomial):tournoi_binomial =
  match tb with 
  | Noeud(_, _, enfants) -> Noeud(0, zero, tb::enfants)
  | Feuille(_) -> Noeud(0, zero, [Feuille(zero)])
  | Vide -> Vide
  
let rec creer_arbre_binomial (taille:int):tournoi_binomial =
  match taille with
  | 0 -> Feuille(zero)
  | n -> concat_le_double_arbre (creer_arbre_binomial (n-1)) 



(* Opérations sur tournoi binomial *)

let cle_en_tb (cle:cle):tournoi_binomial =
  Feuille(cle)
let tb_en_fb (tb:tournoi_binomial) :file_binomial =
  [tb]

let cle_en_fb (cle:cle):file_binomial =
  tb_en_fb (cle_en_tb cle)


let union2tid (tb1:tournoi_binomial) (tb2:tournoi_binomial) : tournoi_binomial =
  match tb1, tb2 with
  | Noeud(degre1, tb1_cle, tb1_enfants), Noeud(degre2, tb2_cle, tb2_enfants) -> 
     if degre1 <> degre2 then failwith "Les tournois sont de tailles différentes"
     else
     if inf tb1_cle tb2_cle then
       let enfants = tb1_enfants in
       Noeud(degre1+1, tb1_cle, tb2::enfants) 
     else
       let enfants = tb2_enfants in
       Noeud(degre1+1, tb2_cle, tb1::enfants) 
  | Feuille(tb1_cle), Feuille(tb2_cle) ->
     if inf tb1_cle tb2_cle then
       Noeud(1, tb1_cle, [tb2]) 
     else
       Noeud(1, tb2_cle, [tb1]) 
  | _ -> failwith "rien à unir, ou bien pas identique"

let rec deg (tb:tournoi_binomial):int =
  match tb with
  | Noeud(deg, _, _) -> deg
  | Feuille(_) -> 0
  | Vide -> failwith "anormal"
               
(* la tête de la fb  est le tournoi binomial avec le degré minimum*)
let ajout_min_fb  (tb:tournoi_binomial) (fb:file_binomial): file_binomial =
  tb::fb

let est_vide (tb:tournoi_binomial):bool = 
  match tb with
  | Vide -> true
  | _ -> false

let rec ufret (fb1:file_binomial) (fb2:file_binomial) (retenu: tournoi_binomial)  =
  if est_vide retenu then
    begin
    match fb1, fb2 with
    | [], _ -> fb2
    | _, []-> fb1
    | t1::rst1 , t2::rst2 ->
       let deg1, deg2  = deg t1, deg t2 in
       if deg1 < deg2 then
         ajout_min_fb t1 (union_fb rst1 fb2)
       else if deg2 < deg1 then
         ajout_min_fb t2 (union_fb rst2 fb1)
       else
         ufret rst1 rst2 (union2tid t1 t2)
    end
  else
    let deg_ret = deg retenu in
    match fb1, fb2 with
    | [], _ -> union_fb (tb_en_fb retenu) fb2
    | _, [] -> union_fb (tb_en_fb retenu) fb1
    | t1::rst1, t2::rst2  ->
       let deg1, deg2 = deg t1, deg t2 in
       if deg_ret < deg1 && deg_ret < deg2 then
         ajout_min_fb retenu (union_fb fb1 fb2)
       else if deg_ret == deg1 && deg_ret == deg2 then
         let file_sans_min = ufret rst1 rst2 (union2tid t1 t2) in 
         ajout_min_fb retenu file_sans_min
       else if deg_ret == deg1 && deg_ret < deg2 then
         ufret rst1 fb2 (union2tid t1 retenu)
       else if deg_ret == deg2 && deg_ret < deg1 then
         ufret rst2 fb1 (union2tid t2 retenu)
       else
         failwith "n'est pas censé arrivé"
and union_fb (fb1:file_binomial) (fb2:file_binomial) : file_binomial =
  ufret fb1 fb2 Vide
  
  
let ajout_fb (cle:cle) (fb:file_binomial):file_binomial =
  let fb_a_ajouter = cle_en_fb cle in
  union_fb fb_a_ajouter fb
 
(* let decapite (tb:tournoi_binomial):file_binomial = *)
  (* on prend la racine
    ah bah je crois qu'on renvoie juste l'attribut "enfants" 
    ah mais je met pas le degré
    enfants, c'est une tb list, alors qu'il nous faut une (degre * tb) list
    pour ça va falloir utilisé map
    pas besoin de calculer
    ça prend une complexité de n par contre du coup...
    bon au pire, on va changer

    ouais mais nan, si on fait ça, ça va pas réglé de problème, car on va toujours renvoyer une liste de tournoi binomiaux et pas une (degre * tb) list -> file_binomial

    que faire alors ? est-ce une si bonne idée que de faire (degre * tb) list
   *)
(* let suppr_min_file (file:file_binomial): (cle * file_binomial) =  *)
  (* on est censé trouver le plus petit en parcourant chaque racine
     on retire le tournoi
     on décapite ce tournoi et on obtient une nouvelle file (fonction décapite)
     on fait l'union de cette nouvelle file, ainsi que de notre file de départ sans le tournoi
   *)
  
  

(* Test : voir si les arbres se construisent avec la bonne taille *)
let exemple_arbre = creer_arbre_binomial 4;;
afficher_nb_noeud_par_niveau exemple_arbre;;



