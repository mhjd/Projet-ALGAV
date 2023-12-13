open Cle
open Structure_de_donnee
open Tas

(* https://v2.ocaml.org/manual/moduleexamples.html *)
module type Graphique  = functor (Ma_structure : Data_structure) -> sig
  val creer_graphique_ajouts_iteratifs :  string -> unit
  val creer_graphique_construction :   string -> unit
end;; 
  


module Graphique : Graphique = functor (Ma_structure : Data_structure) ->  struct 
let string_en_cle (string_en_cle:string) =
(* il y a certain nombre hexadécimaux ayant moins de 32 chiffre, il faut donc ajouter des 0 devant : *)
  (let string_en_string32 =
     (* "0x" + 32 octets = 34 octets *)
     if String.length string_en_cle < 34 then
       ( let nb_caractere_en_moins = 34 - String.length string_en_cle in 
         let nb_caractere = (32 - nb_caractere_en_moins) in
         let nb_0 = String.make nb_caractere_en_moins '0' in
         (* 2 : on retire le 0x au début *)
         "0x" ^ nb_0  ^ String.sub string_en_cle 2 nb_caractere
       )
     else
       string_en_cle
   in
   let mes_bytes_en_string = List.map (fun i -> String.sub string_en_string32 (2+i*8) 8) [0; 1; 2; 3] in
   let mes_bytes_en_int32 = List.map (fun byte_en_string -> Int32.of_string ("0x"^byte_en_string)) mes_bytes_en_string in
   match mes_bytes_en_int32 with
   | [el1; el2; el3; el4] -> (el1, el2, el3, el4)
   (* je crois que les cas suivants  sont inutiles, je laisse au cas où*)
   (* | [el2; el3; el4] -> (Int32.zero, el2, el3, el4) *)
   (* | [el3; el4] -> (Int32.zero, Int32.zero, el3, el4) *)
   (* | [el4] -> (Int32.zero, Int32.zero, Int32.zero, el4) *)
   | _ -> failwith "anormal, il est censé y avoir 4 entiers"

  )
let fichier_vers_liste_cle (fichier:string):cle list =
  let channel = open_in fichier in
  let rec lire channel (liste_cle_str:cle list) =
    (try
       match input_line channel with
       | exception End_of_file -> liste_cle_str
       | el -> lire channel ((string_en_cle el)::liste_cle_str) 
     with e ->
       close_in_noerr channel ;
       raise e
    ) in lire channel []

let temps fonc cle_fichier:float =
  let debut = Sys.time() in
  let _ = fonc cle_fichier in
  let fin = Sys.time() in
  fin -. debut

let moyenne_temps (fonc: (cle list -> Ma_structure.structure)) (taille:int) =
  let noms_fichier:string list = List.map (fun i -> Printf.sprintf "./cles_alea/cles_alea/jeu_%d_nb_cles_%d.txt" i taille) [1; 2; 3; 4; 5] in
  (* liste contenant les listes de clés de chaque fichier *)
  let liste_cle_fichier:cle list list = List.map (fun fichier -> fichier_vers_liste_cle fichier) noms_fichier in
  let temps_cle:float list = List.map (fun cles_fichier -> temps (fonc) cles_fichier)  liste_cle_fichier in
  let moyenne = (List.fold_left (+.) 0.0 temps_cle) /. 5.0 in
  moyenne


let tailles = [1000 ; 5000 ; 10000 ; 20000 ; 50000 ; 80000 ; 120000 ; 200000]

let liste_temps_tailles (fonc: (cle list -> Ma_structure.structure)) =
  List.map (fun x -> (moyenne_temps fonc x, x)) tailles



(* si on créer plusieurs fonction, une par type de chose qu'on veut tester (union, cons, etc), bah on pourra directement mettre en type de paramètre un attribut de notre Structure *)
(* en faite, on aurait même pas besoin de passer la fonction en paramètre *)



let creer_graphique_ajouts_iteratifs (f_sortie:string) =
  let temps_moyens = liste_temps_tailles Ma_structure.ajouts_iteratifs in
  let file = open_out f_sortie in
  (* y a peut-être pas moyen de direct pattern matché en argument (temps, taille) *)

  List.iteri (fun i (temps, taille) ->  Printf.fprintf file "%d %f\n" taille temps) temps_moyens

let creer_graphique_construction (f_sortie:string) =
  let temps_moyens = liste_temps_tailles Ma_structure.construction in
  let file = open_out f_sortie in
  (* y a peut-être pas moyen de direct pattern matché en argument (temps, taille) *)

  List.iteri (fun i (temps, taille) ->  Printf.fprintf file "%d %f\n" taille temps) temps_moyens
;;
end  ;;

module Graphe_tas_min = Graphique (Tas_min_tab)
