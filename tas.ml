open Cle
open Structure_de_donnee

module Tas_min_tab : Data_structure = struct

type taille = int ref
type tableau = cle option array ref
type structure =  tableau * taille


let taille_allouer_tas (tas : structure) =  Array.length !(fst tas)
let taille_tas (tas : structure):int = !(snd tas)

let afficher_tas (tas: structure) =
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

let set_tab (tas:structure) (el_i:int) (el:cle option) =
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
    
let get_tab (tas:structure) (el_i:int) =
  (* si l'élément dépasse la taille du tas *)
  if Array.length !(fst tas) <= el_i then
    (* on génère pas d'erreur, car dans descendre_el, ça peut être utile dans certain cas, comme lorsque l'on a presque remplit le tas, mais c'est pas une puissance de 2
     et en plus c'est assez logique vu qu'on utilise un array, les valeur None ne sont pas censer exister de toute façon pour l'utilisateur*)
    None
  else
    Array.get !(fst tas) el_i

  
let echanger_el_tab (tas : structure) (el1_i:int) (el2_i:int) =
  (* print_string "le soucis vient bien de là \n" ; *)
  let taille = taille_allouer_tas tas in 
  if el1_i >= taille || el2_i >= taille then failwith "Erreur : les indices des éléments de l'échange dépasse la taille du tas"
  else if el1_i = 0 && el2_i = 0 then
    ((* print_string "on rentre ici \n" ; *) set_tab tas 0 None (* ; print_string "et on sort\n" *) ; ())
  else 
    (
  (* Printf.printf " el 1 %d, el 2 %d \n" el1_i el2_i ;  *)
  let val_el1 = get_tab tas el1_i in
  let val_el2 = get_tab tas el2_i in
  match val_el1, val_el2 with
  | None, None -> failwith "Erreur : les éléments à échanger n'existent pas"
  | el1, el2 -> (set_tab tas el1_i el2); set_tab tas el2_i el1
    )


(* permet de descendre un élément pour la construction
   La différence avec la descente de la supressions, est que pour la supression, on possède l'élément le plus grand
 Alors que là, on souhaite s'arrêter lorsque les enfants sont plus grand*)
let rec descendre_el_cons tas el_i =
    let fils_g_i, fils_d_i  = el_i*2+1, el_i*2+2  in
    let fils_g_opt, fils_d_opt  = get_tab tas fils_g_i, get_tab tas fils_d_i in
    let pere_opt = get_tab tas el_i in
    match fils_g_opt, fils_d_opt, pere_opt with 
    | None, None, _ ->  () (* on est arrivé au bout *)
    | Some(g), None, Some(el) -> (* on est arrivé au bout*)
       if (inf g el) then echanger_el_tab tas el_i fils_g_i 
       else () 
         
    | Some(g), Some(d), Some(el) ->
    (* s'il y a encore un élément inférieur en dessous, on continue à descendre*)
    (if inf d el || inf g el then 
       let d_ou_g = (if inf d g then fils_d_i
                     else fils_g_i) in 
       echanger_el_tab tas el_i d_ou_g ;
       descendre_el_cons tas d_ou_g
    )
    | _, _, _ -> failwith "anormal, cas impossible"

let rec descendre_el tas el_i =
  (* 2i + 1 : fils g, 2i + 2 : fils d *)
    let fils_g_i, fils_d_i = (el_i*2+1, el_i*2+2) in
    let el_opt = get_tab tas el_i in 
    let fils_g_opt, fils_d_opt = get_tab tas fils_g_i, get_tab tas fils_d_i in
    (* afficher_tas tas; *)
    match el_opt, fils_g_opt, fils_d_opt with 
    | _, None, None -> ()     (* on est arrivé aux feuilles *)
    | _, None, Some(supr) -> failwith "cas impossible" (* gauche à droite au feuille, dans un tas *)
    | Some(el), Some(supr), None  -> (if inf supr el then (echanger_el_tab tas el_i fils_g_i))
    | Some (el), Some(g), Some(d)  ->
      ( if inf g el ||  inf d el then
       (let fils_min = (if inf d g then fils_d_i else fils_g_i) in (* on récupère le min *)
        (* print_string "le soucient \n"; *)
       echanger_el_tab tas el_i fils_min ; (* on l'échange de place avec le noeud courant, le plus grand du tas *)
       descendre_el tas fils_min)
      )
    | None , _, _ -> failwith "cas impossible"
       

let rec monter_el (tas:structure) (el_i:int) =
  if el_i = 0 then () (* l'élément est en tête, plus rien à faire *)
  else
    let parent_i = (el_i-1)/2 in (* accès au parent : (i-1)/2 *)
    let el_opt, parent_opt = get_tab tas el_i, get_tab tas parent_i in 
    match el_opt, parent_opt with
    | Some(el), Some(parent) -> ( 
      if inf el parent then 
        (echanger_el_tab tas el_i parent_i ;
         monter_el tas parent_i)
      else ()
    )
    | _ , _ -> failwith "Erreur : anormal, un None dans la remontée"

let suppr_min (tas_tab:structure) : (cle option * structure option) =
  let tab, taille = tas_tab in
  let plus_petit_el = get_tab tas_tab 0 in
  set_tab tas_tab 0 None ;
  (* afficher_tas tas_tab ;  *)
  (* on déplace le plus grand élément à racine *)
  (* print_string "descendre est pas le soucis\n" ; *)
  (* il faut supprimer le dernier élément en tête,  *)
  (* if !taille-1 = 0 then (None, None) else ( *)
  if (!taille > 1 ) then (
    (* ok j'ai pigé la subtilité je pense, ça doit venir de descendre_el_tas qui fonctionne mal dans le cas où l'on arrive a deux élément peut-être *)
    (* non je pense pas, mais quand on fait un échange avec lui même, normalemnt c'est qu'il reste un seul élément, et c'est le plus grand
       pourquoi on le set à None ? on est censé le récupéré pour le renvoyer, ça serait la chose logique à faire
       Ok, ce qui me parait logique est de faire l'échange si il y a une taille > 1, sinon si il y a une taille de 1 exactement bah on renvoie l'élément courant et taille - 1
       et sinon, c'est qu'il y a aucun élément dedans, et on renvoie None
     *)

  (* print_string "ici soucient" ; *)
  echanger_el_tab tas_tab 0 (!taille-1);
  taille := !taille - 1 ;
  (* on le fait descendre *)
  (* print_string "descendre est le soucis\n" ; *)
  descendre_el tas_tab 0;
  (plus_petit_el, None)
  )
  else if !taille = 1 then

    ( taille := !taille - 1 ;
   (plus_petit_el, None) )
  else
    (
    (None, None))

let ajout (tas_tab:structure) (el:cle) : structure option =
  let taille = (snd tas_tab) in 
  (* on ajoute l'élément à la fin *)
  taille := !taille + 1;
  set_tab tas_tab (!taille-1) (Some(el)) ;
  monter_el tas_tab (!taille-1) ;
  None


  
  
let ajouts_iteratifs (el_liste:cle list) : structure =
  let taille_liste  = List.length el_liste in 
  let tab = Array.make taille_liste None in
  let le_return:structure = (ref tab, ref 0) in
  let rec aux (tas_tab:structure) (el_liste:cle list) =
 ( match el_liste with
  | [] -> ()
  | tete::reste -> let _ = ajout tas_tab tete in
                  aux tas_tab reste
 ) in aux le_return el_liste ;
  le_return 

let construction  (el_liste:cle list)  : structure =
  let el_opt_liste = List.map (fun el -> Some(el)) el_liste in
  let tab =  Array.of_list el_opt_liste in
  let taille_liste  = List.length el_liste in 
  let le_return:structure = (ref tab, ref taille_liste) in
  let rec ranger_vals (tas:structure) (el_i:int) =
    if el_i >= 0 then 
      (descendre_el_cons tas el_i ;
       ranger_vals tas (el_i-1))
    else
      (tas)
  in ranger_vals le_return (taille_liste-1)




let union (tas1:structure) (tas2:structure)=
  (* j'aurais simplement pu faire fst tas1 @ fst tas2, ça aurait été bcp plus simple, et j'additionne les tailles  *)
  let taille1, taille2 = taille_tas tas1, taille_tas tas2 in
  let some_cle_en_cle (cle:cle option):cle=
  match cle with
  | Some(el) -> el
  | None -> failwith "anormal" in 

  let f (i:int):(cle) = (* prend l'élément de tab   *)
    (some_cle_en_cle  (if i < taille1 then get_tab tas1 i else get_tab tas2 (i-taille1) ))  in 
  let res = List.init (taille1 + taille2) f
  in construction res

let rec en_liste_croissante (tas: structure) : cle list =
  (* afficher_tas tas ; *)
  let cle, _ = suppr_min tas in
  match cle with
  | Some(cle) -> cle::(en_liste_croissante tas)
  | None -> []


end;;
