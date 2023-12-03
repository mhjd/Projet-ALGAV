open Cle

type taille = int ref
type tableau = cle option array ref
type tas_min_tab =  tableau * taille

let afficher_tas (tas: tas_min_tab) =
  Printf.printf "Contenu du tas : ";
  Array.iter (fun elem ->
    match elem with
    | Some x -> Printf.printf "%d " (val_cle x)
    | None -> Printf.printf "%d " 0
  ) !(fst tas);
  print_newline ()

(* vérification à faire : que les deux élément ne dépasse pas la taille du tas
   si c'est le cas, eh bien générer une erreur, car on est pas censé faire d'échange avec des None 
 *)

let set_tab (tas:tas_min_tab) (el_i:int) (el:cle option) =
  let tab, _ = tas in
  let taille_tab = Array.length !tab in
  if el_i >= taille_tab then
    (* sachant que el_i > taille_tab, alors ça fait plus du double de l'ancienne taille, et le +1 est là pour le cas où le tableau est vide*)
    let tab_maj = Array.make (el_i*2+1) None in
    Array.blit !tab 0 tab_maj 0 taille_tab ;
    Array.set tab_maj el_i el ;
    tab := tab_maj
  else
    Array.set !tab el_i el 
    
let get_tab (tas:tas_min_tab) (el_i:int) =
  if Array.length !(fst tas) <= el_i then
    (* on génère pas d'erreur, car dans descendre_el, ça peut être utile dans certain cas, comme lorsque l'on a presque remplit le tas, mais c'est pas une puissance de 2
     et en plus c'est assez logique vu qu'on utilise un array, les valeur None ne sont pas censer exister de toute façon pour l'utilisateur*)
    None
  else
    Array.get !(fst tas) el_i

    
  
let echanger_el_tab (tas : tas_min_tab) (el1_i:int) (el2_i:int) =
  let taille = Array.length !(fst tas) in 
  if el1_i >= taille || el2_i >= taille then failwith "Erreur : les indices des éléments de l'échange dépasse la taille du tas"
  else 
  let val_el1 = get_tab tas el1_i in
  let val_el2 = get_tab tas el2_i in
  match val_el1, val_el2 with
  | None, None -> failwith "Erreur : les éléments à échanger n'existent pas"
  (* | None, _ -> failwith "Erreur : le premier élément à échanger n'existe pas" *)
  (* | _, None -> failwith "Erreur : le deuxième élément à échanger n'existe pas" *)
  (* on peut échanger un vrai élément et un None, dans le cas où on supprime un élément, donc j'enlève cette partie du pattern matching *)
  | el1, el2 -> (set_tab tas el1_i el2); set_tab tas el2_i el1

(* les machin_i -> indice
   g = gauche
   d = droite
 *)

let printer_to_supr el_s el_d = 
  let _, _, _, el1 = el_s in 
  let _, _, _, el2 = el_d in
  Printf.printf "Élément échanger : %d, %d\n" (Int32.to_int el1) (Int32.to_int el2)

(* permet de descendre un élément pour la construction
   La différence avec la descente de la supressions, est que pour la supression, on possède l'élément le plus grand
 Alors que là, on souhaite s'arrêter lorsque les enfants sont plus grand*)
let rec descendre_el_cons tas el_i =
    let fils_g_i = el_i*2+1 in
    let fils_d_i = el_i*2+2 in
    let fils_g_opt = get_tab tas fils_g_i in
    let fils_d_opt = get_tab tas fils_d_i in
    let noeud_opt = get_tab tas el_i in
    (* afficher_tas tas; *)
    (* Printf.printf "falshe %d\n" el_i; *)
    match fils_g_opt, fils_d_opt, noeud_opt with 
    | None, None, _ ->  () (* print_string "pas d'enfant\n" *)    (* on est arrivé au bout *)
    (* soucis : on ne descend pas dans tout les cas
     ah bah c'est bon normalement, car même en cas d'échange, on échange avec un Some(x), jamais avec le None*)
    | Some(g), None, Some(el) ->
       if (inf g el) then 
         (
         (* print_string "falshe descente en fin \n"; *)

         (* printer_to_supr el g; *)
         echanger_el_tab tas el_i fils_g_i 
         )
       else 
         (* on est arrivé au bout*)
         ()
    | None, Some(_), _ -> failwith "anormal, cas impossible "
    | Some(g), Some(d), Some(el) ->
    ((* print_string "falshe descente\n"; *)
    if inf d el || inf g el then 
         (
         if inf d g then
           (
           (* printer_to_supr el d; *)
             echanger_el_tab tas el_i fils_d_i ;
            descendre_el_cons tas fils_d_i)
         else if inf g d then
           (
           (* printer_to_supr el g; *)
             echanger_el_tab tas el_i fils_g_i ;
            descendre_el_cons tas fils_g_i)
         )
    )
    | _, _, None -> failwith "anormal, cas impossible"

let rec descendre_el tas el_i =
    let fils_g_i = el_i*2+1 in
    let fils_d_i = el_i*2+2 in

    let fils_g = get_tab tas fils_g_i in
    let fils_d = get_tab tas fils_d_i in
    afficher_tas tas;
    match fils_g, fils_d with 
    | None, None -> ()     (* on est arrivé au bout *)
    (* soucis : on ne descend pas dans tout les cas
     ah bah c'est bon normalement, car même en cas d'échange, on échange avec un Some(x), jamais avec le None*)
    | Some(supr), None ->
       (* printer_to_supr supr; *)
       echanger_el_tab tas el_i fils_g_i ; 
       (* opération inutile, car on est au bout s'il y a un None, à supprimer proprement en faisant des tests *)
       descendre_el tas fils_g_i
    | None, Some(supr) ->
       (* cas impossible, mettre un failwith *)
       (* printer_to_supr supr; *)
       echanger_el_tab tas el_i fils_d_i ;
       descendre_el tas fils_d_i
    | Some(g), Some(d) ->
       (* printer_to_supr g; *)
       if inf d g then
                            (echanger_el_tab tas el_i fils_d_i ;
                            descendre_el tas fils_d_i)
                          else
                            (echanger_el_tab tas el_i fils_g_i ;
                            descendre_el tas fils_g_i)
  (* 2i + 1 fils g
     2i + 2 fils d
     ce qu'onv eut faire, c'est échanger le noeud courant avec l'fils le plus petit 
     il faudrait donc récupérer la valeur min entre les deux 

  pour faire descendre, il faut faire descendre l'élément, jusqu'à atteindre une feuille
     pour le faire descendre, on l'échange avec son fils le plus petit, droit ou gauche, et ce jusqu'à que les deux feuille soient à None (càd plus d'fils
     Y a pas besoin d
   *)
let rec monter_el (tas:tas_min_tab) (el_i:int) =
  if el_i = 0 then
    (* l'élément est en tête, plus rien à faire *)
    ()
  else
    (* https://www.cs.dartmouth.edu/~cs10/notes14.html -> accès au parent calcul *)
  let parent_i = (el_i-1)/2 in
  let parent_opt = get_tab tas parent_i in
  let el_opt = get_tab tas el_i in 
  match el_opt, parent_opt with
  | None , _ -> failwith "1 anormal, il n'est pas censer y avoir de valeur None dans la remontée"
  | _, None -> failwith "2 anormal, il n'est pas censer y avoir de valeur None dans la remontée"
  | Some(el), Some(parent) -> ( 
  (* Printf.printf "  élément à ajouter : %d \n  parent : %d \n" el_i parent_i ; *)
  if inf el parent then
    (echanger_el_tab tas el_i parent_i ;
     monter_el tas parent_i)
  else
    ()
  )
let supprMinTab (tas_tab:tas_min_tab) =
  let tab, taille = tas_tab in
  (* on récupère la plus petite valeur, qui est le premier élément *)
  let retour = get_tab tas_tab 0 in
  set_tab tas_tab 0 None ;
  afficher_tas tas_tab ; 
  (* on déplace le plus grand élément à racine *)
  echanger_el_tab tas_tab 0 (!taille-1);
  taille := !taille - 1 ;
  (* on le fait descendre *)
  descendre_el tas_tab 0;
  retour

let ajoutMinTab (tas_tab:tas_min_tab) (el:cle) =
  let taille = (snd tas_tab) in 
  (* on ajoute l'élément à la fin *)
  taille := !taille + 1;
  set_tab tas_tab (!taille-1) (Some(el)) ;
  monter_el tas_tab (!taille-1)


  
  
let ajoutIteratifsTab (el_list:cle list) (taille_allouer:int) =
  let tab = Array.make taille_allouer None in
  let taille = List.length el_list in
  let le_return:tas_min_tab = (ref tab, ref 0) in
  let rec aux (tas_tab:tas_min_tab) (el_list:cle list) =
 ( match el_list with
  | [] -> ()
  | tete::reste -> ajoutMinTab tas_tab tete ;
                  aux tas_tab reste
 ) in aux le_return el_list ;
  le_return 

let constructionTab  (el_list:cle list) (taille_allouer:int) =
  let el_opt_list = List.map (fun el -> Some(el)) el_list in
  let tab = Array.of_list el_opt_list in
  let taille = Array.length tab in
  let taille_allouer = (max taille_allouer (taille*4)) in 
  let le_return:tas_min_tab = (ref tab, ref taille) in
  let rec ranger_vals (tas:tas_min_tab) (el_i:int) =
    if el_i >= 0 then 
      (
        (* Printf.printf "Étape %d : \n" el_i ;  *)
        descendre_el_cons tas el_i ;
       ranger_vals tas (el_i-1))
    else
      (tas)
  in ranger_vals le_return (taille-1)

let int32_of_int_tuple tuple  =
  let a, b, c, d = tuple in
  (Int32.of_int a, Int32.of_int b, Int32.of_int c, Int32.of_int d)

let conv = int32_of_int_tuple
let some_int32_of_int_tuple tuple  =
  let a, b, c, d = tuple in
  (Some(Int32.of_int a, (Int32.of_int b), (Int32.of_int c), (Int32.of_int d)))
let conv2 = some_int32_of_int_tuple
(* let test_construction =        *)
(*   let tas_initial : tas_min_tab =  (constructionTab [(conv (0,0,0,13)) ; (conv (0,0,0,14)) ; (conv (0,0,0,8)) ; (conv (0,0,0,15)) ; (conv (0,0,0,2)) ; (conv (0,0,0,7)) ; (conv (0,0,0,5)) ; (conv (0,0,0,12)) ; (conv (0,0,0,10)) ; (conv (0,0,0,6))] 0) in  *)
(*   let tas_initial_2 : tas_min_tab = (constructionTab [conv (0, 0, 0, 18); conv (0, 0, 0, 1); conv (0, 0, 0, 23); conv (0, 0, 0, 12); conv (0, 0, 0, 38); conv (0, 0, 0, 3); conv (0, 0, 0, 32); conv (0, 0, 0, 6); conv (0, 0, 0, 22); conv (0, 0, 0, 30); conv (0, 0, 0, 29); conv (0, 0, 0, 7); conv (0, 0, 0, 14); conv (0, 0, 0, 27); conv (0, 0, 0, 28); conv (0, 0, 0, 40); conv (0, 0, 0, 19); conv (0, 0, 0, 10); conv (0, 0, 0, 25); conv (0, 0, 0, 39) *)
(* ] 0)  in *)
(*   afficher_tas tas_initial_2 *)



   (* Définition de la fonction *)
let afficher_dernier_element_liste liste =
  let afficher_option_elem opt_elem =
    match opt_elem with
    | Some (_, _, _, dernier) -> Printf.printf "%d " dernier
    | None -> Printf.printf "N/A "
  in
  List.iter afficher_option_elem liste;
  Printf.printf "\n"
;; 
let union_tas_tab (tas1:tas_min_tab) (tas2:tas_min_tab)=
  let tab1, taille1 = tas1 in
  let tab2, taille2 = tas2 in
  let f (i:int):(cle) = match (if i < !taille1 then (get_tab tas1 i) else (get_tab tas2 (i-(!taille1)))) with Some(el) -> el | None -> failwith "anormal" in
  let res = List.init (!taille1 + !taille2) f in
  constructionTab res 0
  (* (ref res, ref (!taille1 + !taille2)) *)

(* let test_union_tas_tab () = *)
(*   let tas1 = constructionTab [conv (0, 0, 0, 37);  conv (0, 0, 0, 18); conv (0, 0, 0, 15); conv (0, 0, 0, 39); conv (0, 0, 0, 11); conv (0, 0, 0, 40); conv (0, 0, 0, 27); conv (0, 0, 0, 25); conv (0, 0, 0, 9); conv (0, 0, 0, 32); conv (0, 0, 0, 3); conv (0, 0, 0, 19); conv (0, 0, 0, 23); conv (0, 0, 0, 10); conv (0, 0, 0, 29); conv (0, 0, 0, 14); conv (0, 0, 0, 26); conv (0, 0, 0, 4)] 0 in *)
(*   let tas2 = constructionTab [conv (0, 0, 0, 18); conv (0, 0, 0, 28); conv (0, 0, 0, 35); conv (0, 0, 0, 8); conv (0, 0, 0, 36); conv (0, 0, 0, 34); conv (0, 0, 0, 16); conv (0, 0, 0, 12)] 0 in *)

(*   Printf.printf "Tas 1 avant l'union : "; *)
(*   afficher_tas tas1; *)
(*   Printf.printf "Taille : %d\n" !(snd tas1); *)

(*   Printf.printf "Tas 2 avant l'union : "; *)
(*   afficher_tas tas2; *)
(*   Printf.printf "Taille : %d\n" !(snd tas2); *)

(*   let tas_union = union_tas_tab tas1 tas2 in *)

(*   Printf.printf "Tas résultant de l'union : "; *)
(*   afficher_tas (tas_union); *)

(*   Printf.printf "Taille attendue de l'union : %d\n" (!(snd tas1) + !(snd tas2)); *)

(*   Printf.printf "Contenu attendu de l'union : "; *)
(*   Array.iter (fun elem -> *)
(*     match elem with *)
(*     | Some x -> Printf.printf "%d " (val_cle x) *)
(*     | None -> Printf.printf "%d " 0 *)
(*   ) !(fst tas_union); *)
(*   print_newline () *)
(* ;; *)

(* Appel du test *)
(* test_union_tas_tab (); *)
