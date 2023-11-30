
(* 32 bits car dans l'exo MD5 utilise des entier 32 bits, donc peut-être plus pertinent, au nez*)

type cle = int32 * int32 * int32 * int32 


(* unsigned_compare compare deux entiers 32 bits et retourne un entier qui représente l'ordre relatif des deux valeurs. Si le premier entier est inférieur au deuxième, la fonction retournera un entier négatif. Si le premier entier est égal au deuxième, elle retournera zéro. Si le premier entier est supérieur au deuxième, elle retournera un entier positif. *)
(* Merci à Danaël pour l'astuce *)
let inf_int32 a b = Int32.unsigned_compare a b < 0 
  
let eq_int32 a b = Int32.unsigned_compare a b = 0 
let neq_int32 a b = not (eq_int32 a b)


let inf_naif (cle_a:cle) (cle_b:cle) : bool =
  let entier1a, entier2a, entier3a, entier4a = cle_a in
  let entier1b, entier2b, entier3b, entier4b = cle_b in
  (* si le premier entier est inférieur, renvoyer true car c'est la partie "la plus à gauche", sinon false
     Comme dans le nombre 1234 et 4234, on voit que 1 < 4, donc on renvoie que c'est inférieur
   *)
  if inf_int32 entier1a entier1b then
    true
  else if inf_int32 entier1b entier1a  then
    false
  (* si on arrive là, c'est que les deux premier nombre sont égaux, donc on va regarder plus loin pour voir une infériorité
     Comme dans le nombre 1234 et 1334, on voit que les deux premiers nombres sont égaux, donc on regarde les deux deuxième, et  2 < 3, donc on renvoie que c'est inférieur
     c'est comme ça à chaque fois
     Je sais pas si c'est la méthode la plus opti 
   *)
  else if inf_int32 entier2a entier2b then
    true 
  else if inf_int32 entier2b entier2a  then
    false
  else if inf_int32 entier3a entier3b then
    true
  else if inf_int32 entier3b entier3a  then
    false
  else if  inf_int32 entier4a entier4b then
    true
  else if  inf_int32 entier4b entier4a  then
    false
  else
    false

(* fonction plus propre et qui marche tout aussi bien je pense.
 En gros, on passe à l'int32 suivant (plus à droite) seulement si les deux premiers sont égaux, sinon on renvoie directement inf_int32 entierXa entierXb, car ils sont inégaux donc la réponse se trouve là *)
let inf (cle_a:cle) (cle_b:cle) : bool =
  let entier1a, entier2a, entier3a, entier4a = cle_a in
  let entier1b, entier2b, entier3b, entier4b = cle_b in
  if neq_int32 entier1a entier1b then
    inf_int32 entier1a entier1b
  else if neq_int32 entier2a entier2b then
    inf_int32 entier2a entier2b
  else if neq_int32 entier3a entier3b then
    inf_int32 entier3a entier3b
  else if neq_int32 entier4a entier4b then
    inf_int32 entier4a entier4b
  else
    false 
         
(* test :  
   renvoie false  : 

let a:cle = (Int32.of_int 4, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  6) in
    inf a b

    renvoie true :
    
let a:cle = (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  6) in
    inf a b

*)
let eg (cle_a:cle) (cle_b:cle) : bool =
  cle_a = cle_b

(* test :  
   renvoie true: 

let a:cle = (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in
    eg a b

    renvoie false :
    
let a:cle = (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  6) in
    eg a b

*)

type taille = int ref
type tableau = int option array ref
type tas_min_tab = tableau * taille

let afficher_tas (tas: tas_min_tab) =
  Printf.printf "Contenu du tas : ";
  Array.iter (fun elem ->
    match elem with
    | Some x -> Printf.printf "%d " x
    | None -> ()
  ) !(fst tas);
  print_newline ()


(* vérification à faire : que les deux élément ne dépasse pas la taille du tas
   si c'est le cas, eh bien générer une erreur, car on est pas censé faire d'échange avec des None 
 *)

let set_tab (tas:tas_min_tab) (el_i:int) (el:int option) =
  let tab, _ = tas in
  let taille_tab = Array.length !tab in
  if el_i >= taille_tab then
    let tab_maj = Array.make (max (taille_tab*2) el_i*2) None in
    Array.blit !tab 0 tab_maj 0 taille_tab ;
    Array.set tab_maj el_i el ;
    tab := tab_maj
  else
    Array.set !tab el_i el 
    
let get_tab (tas:tas_min_tab) (el_i:int) =
  if Array.length !(fst tas) < el_i then
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

let rec descendre_el tas el_i =
    let fils_g_i = el_i*2+1 in
    let fils_d_i = el_i*2+2 in

    let fils_g = get_tab tas fils_g_i in
    let fils_d = get_tab tas fils_d_i in
    match fils_g, fils_d with 
    | None, None -> ()     (* on est arrivé au bout *)
    | Some(_), None -> echanger_el_tab tas el_i fils_g_i ; 
afficher_tas tas;
                       descendre_el tas fils_g_i
    | None, Some(_) -> echanger_el_tab tas el_i fils_d_i ;
afficher_tas tas;
                       descendre_el tas fils_d_i
    | Some(g), Some(d) -> if g > d then
                            (echanger_el_tab tas el_i fils_d_i ;
afficher_tas tas;
                            descendre_el tas fils_d_i)
                          else
                            (echanger_el_tab tas el_i fils_g_i ;
afficher_tas tas;
                            descendre_el tas fils_g_i)
  (* 2i + 1 fils g
     2i + 2 fils d
     ce qu'onv eut faire, c'est échanger le noeud courant avec l'fils le plus petit 
     il faudrait donc récupérer la valeur min entre les deux 

  pour faire descendre, il faut faire descendre l'élément, jusqu'à atteindre une feuille
     pour le faire descendre, on l'échange avec son fils le plus petit, droit ou gauche, et ce jusqu'à que les deux feuille soient à None (càd plus d'fils
     Y a pas besoin d
   *)

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


let () =
  (* Création d'un tas *)
  let tas_initial : tas_min_tab = (ref [|Some 2 ; Some 6 ; Some 5 ; Some 10 ; Some 13 ; Some 7 ; Some 8 ; Some 12 ; Some 15 ; Some 14|], ref 10) in

  (* Affichage du tas initial *)
  afficher_tas tas_initial;

  (* Suppression du minimum *)
  let min_supprime = supprMinTab tas_initial in

  (* Affichage du tas après suppression *)
  afficher_tas tas_initial;

  (* Affichage de la valeur supprimée *)
  Printf.printf "Valeur supprimée : %d\n" (match min_supprime with Some x -> x | None -> 0)
