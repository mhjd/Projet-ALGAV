(* 32 bits car dans l'exo MD5 utilise des entier 32 bits, donc peut-être plus pertinent, au nez*)

type cle = int32 * int32 * int32 * int32 

let val_cle (la_cle:cle) =
  let _, _, _, val_min = la_cle in
  Int32.to_int val_min

(* unsigned_compare compare deux entiers 32 bits et retourne :
   - un nombre négatif si premier entier inférieur au second
   - positif si supérieur
   - 0 si c'est les même nombre . *)
(* Merci à Danaël pour l'astuce *)
let inf_int32 a b = (Int32.unsigned_compare a b) < 0 
let eq_int32 a b = (Int32.unsigned_compare a b) = 0 
let neq_int32 a b = not (eq_int32 a b)


(* on passe à l'int32 suivant (plus à droite) ssi les deux premiers sont égaux, sinon on renvoie directement inf_int32 entierXa entierXb, car ils sont inégaux donc la réponse se trouve là *)
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
         
let eg (cle_a:cle) (cle_b:cle) : bool =
  cle_a = cle_b


(* test inf :  
   renvoie false  : 

let a:cle = (Int32.of_int 4, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  6) in
    inf a b

    renvoie true :
    
let a:cle = (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  6) in
    inf a b

*)
(* test eg :  
   renvoie true: 

let a:cle = (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in
    eg a b

    renvoie false :
    
let a:cle = (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  4) in let b:cle = 
    (Int32.of_int 1, Int32.of_int 2, Int32.of_int 3, Int32.of_int  6) in
    eg a b

*)
