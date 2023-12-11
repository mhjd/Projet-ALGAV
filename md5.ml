(* On crée des opérateurs pour que la suite des opérations soient plus lisibles
pour nous *)
let (>>) x n = x lsr n
let (<<) x n = x lsl n
let (&&&) x y = x land y
let (|||) x y = x lor y
let (^^^) x y = x lxor y
let not = lnot

let r = Array.of_list [7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22; 7; 12; 17; 22;
                       5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20; 5; 9; 14; 20;
                       4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23; 4; 11; 16; 23;
                       6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21; 6; 10; 15; 21]

(* On génère 64 mots de 32 bits pour MD5 *)
let k = Array.init 64 (fun i -> int_of_float (abs_float (sin (float_of_int (i + 1))) *. 2. ** 32.))

(* Simple fonction permettant de concaténer 4 mots de 32 bits *)
let concatener a b c d = (a lsl 96) lor (b lsl 64) lor (c lsl 32) lor d
(* =============================================================================================== *)

                         
(* On prédéfinit les valeurs du hachage *)
let h0 = 0x67452301
let h1 = 0xEFCDAB89
let h2 = 0x98BADCFE
let h3 = 0x10325476
(* =============================================================================================== *)

(* Fonction pour  *)
let leftrotate x n =
  (x << n) ||| (x >> (32 - n))
(* =============================================================================================== *)              
               
(* Fonction pour ajouter le bit 1 à la fin du message *)
let append_bit1 message = (message lsl 1) lor 1 
(* =============================================================================================== *)          
   
(* Fonction permettant le padding avec tous les zéros *)            
let rec append_zeros message =
  let length = message lsl 1 in
  if length mod 512 = 448 then
    message
  else
    append_zeros (message lsl 1)
(* =============================================================================================== *)

(* Fonction principale pour hacher les valeurs *)
let md5 message =
  
  let message = append_bit1 message in
  let message = append_zeros message in
  
  let a = ref h0 in
  let b = ref h1 in
  let c = ref h2 in
  let d = ref h3 in
  
  (* Boucle principale du MD5
  où l'on change les états internes de chacun des mots *)
  for i = 0 to 63 do
    let f, g = if i <= 15 then
        (!b &&& !c) ||| ((not !b) &&& !d), i
      else if i <= 31 then
        (!d &&& !b) ||| ((not !d) &&& !c), (5 * i + 1) mod 16
      else if i <= 47 then
        !b ^^^ !c ^^^ !d, (3 * i + 5) mod 16
      else
        !c ^^^ (!b ||| (not !d)), (7 * i) mod 16
    in
    let temp = !d in
    d := !c;
    c := !b;
    b := !b + leftrotate (!a + f + k.(i) (*+ w[g]*)) r.(i);
    a := temp;
  done;
  let h0 = h0 + !a in
  let h1 = h1 + !b in
  let h2 = h2 + !c in
  let h3 = h3 + !d in
  
  let h = concatener h0 h1 h2 h3 in 
  
  h;
  
(* =============================================================================================== *)
