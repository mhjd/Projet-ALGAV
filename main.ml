
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
