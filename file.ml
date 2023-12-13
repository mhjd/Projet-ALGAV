open Cle 

type degre = int ;;

type tournoi_binomial =
  | Noeud of degre * cle * enfants 
  | Vide
and enfants = tournoi_binomial list

type file_binomiale = tournoi_binomial list

let rec get_enfants (tb:tournoi_binomial):(tournoi_binomial list) =
  match tb with
  | Noeud(_, _, enfants) -> enfants 
  | Vide -> []

let nb_noeud (tb:tournoi_binomial) : int=
  let racine = [tb] in
  let rec aux liste acc =
    let nb_noeud_niveau = List.length liste in
    let nb_noeud = acc + nb_noeud_niveau in
    let enfants = List.flatten (List.map (fun el -> get_enfants el) liste) in
    if enfants <> [] then aux enfants nb_noeud else nb_noeud
  in aux racine 0
;;
let nb_noeud_file (fb:file_binomiale) : int =
  List.fold_left (fun acc tb -> acc + nb_noeud tb ) 0 fb
let rec est_tournoi_binomial (tb:tournoi_binomial) : bool =
  match tb with
  | Noeud(degre, cle, bk_moins_1_gauche::enfants) ->
     let bk_moins_1_droit = Noeud(degre-1, cle, enfants) in
     (nb_noeud tb) = int_of_float (2.0 ** (float_of_int degre)) &&
     (nb_noeud bk_moins_1_gauche) = (nb_noeud bk_moins_1_droit) &&
       est_tournoi_binomial bk_moins_1_droit &&
         est_tournoi_binomial bk_moins_1_gauche
  (* problématique, censé être une feuille, faudrait peut-être se débarasser du concept de feuille et remplacer ça par Vide *)
  | Noeud(0, cle, []) -> true
  | _ -> failwith "n'est vraiment pas un tournoi binomial"

(* Test : voir si les arbres se construisent avec la bonne taille *)
(* let exemple_arbre = creer_arbre_binomial 4;; *)
(* afficher_nb_noeud_par_niveau exemple_arbre;; *)
(* fonction très utile pour voir si l'arbre binomial à bien la bonne forme *)
let afficher_nb_noeud_par_niveau (tb:tournoi_binomial) =
  let racine = [tb] in
  let rec aux liste niveau =
    let nb_noeud = List.length liste in
    Printf.printf "Niveau : %d , Nombre noeud : %d\n" niveau nb_noeud ;
    let enfants = List.flatten (List.map (fun el -> get_enfants el) liste) in
    if enfants <> [] then aux enfants (niveau+1)
  in aux racine 0
;;
let afficher_file_binomial (fb:file_binomiale) =
  List.iter (fun tb -> print_string "autre tb \n " ; afficher_nb_noeud_par_niveau tb) fb ;;

let info_tournoi_binomial (tb:tournoi_binomial) =
  match tb with
  | Noeud (degre, cle, enfant) -> Printf.printf "Arbre de degré : %d\n" degre ; afficher_nb_noeud_par_niveau tb
  | Vide -> Printf.printf "Est vide\n"
  ;; 


(* opérations sur arbre binomial, on commence par ça car plus simple *)

let zero = (Int32.of_int 0,Int32.of_int 0, Int32.of_int 0, Int32.of_int 0) ;;

let concat_le_double_arbre (tb:tournoi_binomial):tournoi_binomial =
  match tb with 
  | Noeud(_, _, enfants) -> Noeud(0, zero, tb::enfants)
  | Vide -> Vide
  
let rec creer_arbre_binomial (taille:int):tournoi_binomial =
  match taille with
  | 0 -> Noeud(0, zero, [])
  | n -> concat_le_double_arbre (creer_arbre_binomial (n-1)) 



(* Opérations sur tournoi binomial *)

let cle_en_tb (cle:cle):tournoi_binomial =
  Noeud(0, cle, [])
let tb_en_fb (tb:tournoi_binomial) :file_binomiale =
  [tb]

let cle_en_fb (cle:cle):file_binomiale =
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
  | _ -> failwith "rien à unir, ou bien pas identique"

let rec deg (tb:tournoi_binomial):int =
  match tb with
  | Noeud(noeud_deg, _, _) -> noeud_deg
  | Vide -> failwith "anormal"
               
(* la tête de la fb  est le tournoi binomial avec le degré minimum*)
let ajout_min_fb  (tb:tournoi_binomial) (fb:file_binomiale): file_binomiale =
  tb::fb

let est_vide (tb:tournoi_binomial):bool = 
  match tb with
  | Vide -> true
  | _ -> false

let rec ufret (fb1:file_binomiale) (fb2:file_binomiale) (retenu: tournoi_binomial)  =
  (* print_string "appel récursif \n" ; *)
  if est_vide retenu then
    (
  (* print_string "pas de retenu \n" ; *)

    begin
    match fb1, fb2 with
    | [], _ -> fb2
    | _, []-> fb1
    | t1::rst1 , t2::rst2 ->    (* erreur viens probablement de t1 et t2, qui ne sont pas les bons *)
       let deg1, deg2  = deg t1, deg t2 in
         (* Printf.printf " deg 1 : %d , deg 2 : %d \n" deg1 deg2 ; *)
       if deg1 < deg2 then
         ajout_min_fb t1 (union_fb rst1 fb2)
       else if deg2 < deg1 then
         ajout_min_fb t2 (union_fb rst2 fb1)
       else if deg2 = deg1 then
         ((* Printf.printf "égaux %d \n" deg1 ; *)
          let mon_uniontid = (union2tid t1 t2) in
          assert(est_tournoi_binomial mon_uniontid) ;
          (* Printf.printf "degré : %d \n" (deg mon_uniontid); *)
         ufret rst1 rst2 (union2tid t1 t2))
       else
         failwith "chelouent"
    end
)
  else
    ((* Printf.printf "retenu %d \n" (deg retenu) ; *)
    let deg_ret = deg retenu in
    match fb1, fb2 with
    | [], _ -> union_fb (tb_en_fb retenu) fb2
    | _, [] -> union_fb (tb_en_fb retenu) fb1
    | t1::rst1, t2::rst2  ->
       let deg1, deg2 = deg t1, deg t2 in
       if deg_ret < deg1 && deg_ret < deg2 then
         ajout_min_fb retenu (union_fb fb1 fb2)
       else if deg_ret = deg1 && deg_ret = deg2 then
         let file_sans_min = ufret rst1 rst2 (union2tid t1 t2) in 
         ajout_min_fb retenu file_sans_min
       else if deg_ret = deg1 && deg_ret < deg2 then
         ufret rst1 fb2 (union2tid t1 retenu)
       else if deg_ret = deg2 && deg_ret < deg1 then
         ufret rst2 fb1 (union2tid t2 retenu)
       (* else if deg1 = 0 && deg2 = 0 then *)
       (*   [retenu] *)
       else
         (* olé olé, pourquoi les degré rapetissent ? noramlemtn même si on prend le reste, eh bien ça doit être des éléments supérieur après *)
         (
         (* Printf.printf "deg_ret : %d , deg 1 : %d , deg 2 : %d \n" deg_ret deg1 deg2 ; *)
         failwith "n'est pas censé arrivé"
         )
    )
and union_fb (fb1:file_binomiale) (fb2:file_binomiale) : file_binomiale =
  ufret fb1 fb2 Vide
  
  
let ajout_fb (cle:cle) (fb:file_binomiale):file_binomiale =
  (* print_string "avant : \n" ;  *)
  (* afficher_file_binomial fb ; *)
  let fb_a_ajouter = cle_en_fb cle in
  let res = union_fb fb_a_ajouter fb in
  (* print_string "après : \n" ; *)
  (* afficher_file_binomial res ; *)
  res
  

let construction_fb (liste_cle : cle list) : file_binomiale =
  (* je suis pas sûr de moi pour l'accumulateur : [] *)
  List.fold_left (fun acc cle -> ajout_fb cle acc) []  liste_cle

let decapite (tb:tournoi_binomial): file_binomiale =
  match tb with
  | Noeud(_, _, enfants) -> List.rev enfants
  | Vide -> failwith "n'est jamais censé arrivé"

let cle_racine (tb:tournoi_binomial):cle =
  match tb with
  | Noeud(_, cle, _) -> cle
  | Vide -> failwith "anormal" 

(* potentiellement, il peut y avoir un problème avec i, on arrête à i=0 ou 1 ? *)
let enlever_i (liste:file_binomiale) (i:int) : (file_binomiale * tournoi_binomial)  =
  let rec aux liste i ret = 
  match liste with
  | h::t -> if i > 0 then let nouveau_t, retour = (aux t (i-1) ret) in (h::nouveau_t, retour)  else t, Some(h)
  | [] -> [], ret
  in
  let nouvelle_fb, tb_opt = aux liste i None in
  match tb_opt with
  | None -> failwith "aucun i n'a été trouvé"
  | Some(tb) -> (nouvelle_fb, tb)
  
let rec est_file_binomiale (fb:file_binomiale) : bool =
  match fb with
  | tb::rst -> est_tournoi_binomial tb && est_file_binomiale rst
  | [] -> true
let suppr_min_file (fb:file_binomiale): (cle * file_binomiale) =
  (* on est censé trouver le plus petit en parcourant chaque racine
     on retire le tournoi
     on décapite ce tournoi et on obtient une nouvelle file (fonction décapite)
     on fait l'union de cette nouvelle file, ainsi que de notre file de départ sans le tournoi
   *)
  let compare_racine plus_petit tb =
      let i, cle_tbi = plus_petit in
      if inf (cle_racine tb) cle_tbi then (i+1, cle_racine tb)
      else plus_petit
  in
  match fb with
  | tb0::reste -> let i , cle_plus_petite = List.fold_left compare_racine (0, cle_racine tb0) fb in
                 (* exit(1) ; *)
                  let fb_sans_i, tb_suppr = enlever_i fb i in 
                  let fb_tb_decapite = decapite tb_suppr in
                  (* Printf.printf "La clé la plus petite est : %d , La taille : %d \n " (val_cle cle_plus_petite) (nb_noeud_file fb_sans_i + nb_noeud_file fb_tb_decapite); *)
                   assert(est_file_binomiale fb_sans_i) ;
                   (* print_string "dans supr\n" ; *)
                  assert(est_file_binomiale fb_tb_decapite) ;
                  (cle_plus_petite, union_fb fb_sans_i fb_tb_decapite)
  | [] -> failwith "file vide, aucun élément à supprimer"
  
  
  
let rec file_en_liste_decroissante (fb:file_binomiale) : cle list =
  if fb <> [] then 
    let cle, fb_moins_1 = suppr_min_file fb in
    cle::(file_en_liste_decroissante fb_moins_1)
  else
    []
