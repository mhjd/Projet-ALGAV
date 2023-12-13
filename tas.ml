open Cle

type taille = int ref
type tableau = cle option array ref
type tas_min_tab =  tableau * taille


let taille_allouer_tas (tas : tas_min_tab) =  Array.length !(fst tas)
let taille_tas (tas : tas_min_tab):int = !(snd tas)

let afficher_tas (tas: tas_min_tab) =
  Printf.printf "Contenu du tas : ";
  Array.iter (fun elem ->
    match elem with
    | Some x -> Printf.printf "%d " (val_cle x)
    | None -> Printf.printf "%d " 0
  ) !(fst tas);
  print_newline ()

let renvoyer_tas (tas: tas_min_tab) =
  Array.fold_left (fun ma_string elem ->
    match elem with
    | Some x -> ma_string ^ (string_of_int (val_cle x ) ) ^ " "
    | None -> ma_string
  ) "" !(fst tas)


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
  (* si l'élément dépasse la taille du tas *)
  if Array.length !(fst tas) <= el_i then
    (* on génère pas d'erreur, car dans descendre_el, ça peut être utile dans certain cas, comme lorsque l'on a presque remplit le tas, mais c'est pas une puissance de 2
     et en plus c'est assez logique vu qu'on utilise un array, les valeur None ne sont pas censer exister de toute façon pour l'utilisateur*)
    None
  else
    Array.get !(fst tas) el_i

  
let echanger_el_tab (tas : tas_min_tab) (el1_i:int) (el2_i:int) =
  let taille = taille_allouer_tas tas in 
  if el1_i >= taille || el2_i >= taille then failwith "Erreur : les indices des éléments de l'échange dépasse la taille du tas"
  else 
  let val_el1 = get_tab tas el1_i in
  let val_el2 = get_tab tas el2_i in
  match val_el1, val_el2 with
  | None, None -> failwith "Erreur : les éléments à échanger n'existent pas"
  | el1, el2 -> (set_tab tas el1_i el2); set_tab tas el2_i el1


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
    let fils_g_opt, fils_d_opt = get_tab tas fils_g_i, get_tab tas fils_d_i in
    (* afficher_tas tas; *)
    match fils_g_opt, fils_d_opt with 
    | None, None -> ()     (* on est arrivé aux feuilles *)
    | None, Some(supr) -> failwith "cas impossible" (* gauche à droite au feuille, dans un tas *)
    | Some(supr), None -> echanger_el_tab tas el_i fils_g_i  
    | Some(g), Some(d) ->
       (let fils_min = (if inf d g then fils_d_i else fils_g_i) in (* on récupère le min *)
       echanger_el_tab tas el_i fils_min ; (* on l'échange de place avec le noeud courant, le plus grand du tas *)
       descendre_el tas fils_min)

let rec monter_el (tas:tas_min_tab) (el_i:int) =
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

let supprMinTab (tas_tab:tas_min_tab) =
  let tab, taille = tas_tab in
  let plus_petit_el = get_tab tas_tab 0 in
  set_tab tas_tab 0 None ;
  (* afficher_tas tas_tab ;  *)
  (* on déplace le plus grand élément à racine *)
  echanger_el_tab tas_tab 0 (!taille-1);
  taille := !taille - 1 ;
  (* on le fait descendre *)
  descendre_el tas_tab 0;
  plus_petit_el

let ajoutMinTab (tas_tab:tas_min_tab) (el:cle) =
  let taille = (snd tas_tab) in 
  (* on ajoute l'élément à la fin *)
  taille := !taille + 1;
  set_tab tas_tab (!taille-1) (Some(el)) ;
  monter_el tas_tab (!taille-1)


  
  
let ajoutIteratifsTab (el_liste:cle list) : tas_min_tab =
  let taille_liste  = List.length el_liste in 
  let tab = Array.make taille_liste None in
  let le_return:tas_min_tab = (ref tab, ref 0) in
  let rec aux (tas_tab:tas_min_tab) (el_liste:cle list) =
 ( match el_liste with
  | [] -> ()
  | tete::reste -> ajoutMinTab tas_tab tete ;
                  aux tas_tab reste
 ) in aux le_return el_liste ;
  le_return 

let constructionTab  (el_liste:cle list)  : tas_min_tab =
  let el_opt_liste = List.map (fun el -> Some(el)) el_liste in
  let tab =  Array.of_list el_opt_liste in
  let taille_liste  = List.length el_liste in 
  let le_return:tas_min_tab = (ref tab, ref taille_liste) in
  let rec ranger_vals (tas:tas_min_tab) (el_i:int) =
    if el_i >= 0 then 
      (descendre_el_cons tas el_i ;
       ranger_vals tas (el_i-1))
    else
      (tas)
  in ranger_vals le_return (taille_liste-1)



let some_cle_en_cle (cle:cle option):cle=
  match cle with
  | Some(el) -> el
  | None -> failwith "anormal"

let union_tas_tab (tas1:tas_min_tab) (tas2:tas_min_tab)=
  let taille1, taille2 = taille_tas tas1, taille_tas tas2 in
  let f (i:int):(cle) = (* prend l'élément de tab   *)
    (some_cle_en_cle  (if i < taille1 then get_tab tas1 i else get_tab tas2 (i-taille1) ))  in 
  let res = List.init (taille1 + taille2) f
  in constructionTab res
