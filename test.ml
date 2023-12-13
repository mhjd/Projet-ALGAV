open Cle
open Tas
open File
open Graphe

     
let rand_int n = Random.int n

let int32_tuple_of_int x =
  (Int32.zero, Int32.zero, Int32.zero, Int32.of_int x)
let conv = int32_tuple_of_int

let test_tas_min () =
  
  (* test ajout itératif *)
  (* Création d'un tas *)
  let tas_ajout_iteratif : tas_min_tab =  ajoutIteratifsTab [conv 6 ; conv 5  ; conv 2 ; conv 10 ; conv 13 ; conv 7 ; conv 8 ; conv 12 ; conv 15 ; conv 14]  in

  (* Affichage du tas ajout_iteratif *)
  assert(((renvoyer_tas tas_ajout_iteratif) = "2 6 5 10 13 7 8 12 15 14 ")) ;

  (* test suppr *)
  (* Suppression du minimum *)
  let min_supprime = supprMinTab tas_ajout_iteratif in
  assert(min_supprime = Some(conv 2)) ;
  assert((renvoyer_tas tas_ajout_iteratif) = "5 6 7 10 13 14 8 12 15 ")  ;
  (* test  construction *)


  (* test cons *)
  let tas_cons : tas_min_tab =  (constructionTab [conv 10; conv 15; conv 13 ; conv 14; conv 8 ; conv 5 ; conv 6 ; conv 7; conv 12] ) 
  in
  assert(renvoyer_tas tas_cons = ("5 7 6 12 8 13 10 14 15 ")) ;

  (* test union *)

  let tas1 = constructionTab [conv 37;  conv 18; conv 15; conv 39; conv 11; conv 40; conv 27; conv 25; conv 9; conv 32; conv 3; conv 19; conv 23; conv 10; conv 29; conv 14; conv 26; conv 4]  in

  let tas2 = constructionTab [conv 18; conv 28; conv 35; conv 8; conv 36; conv 34; conv 16; conv 12] in

  assert((renvoyer_tas tas1) = "3 4 10 9 11 19 15 14 37 32 18 40 23 27 29 25 26 39 ") ; 
  assert((renvoyer_tas tas2) = "8 12 16 18 36 34 35 28 ") ; 
  let union_tas = (union_tas_tab tas1 tas2) in
  assert(renvoyer_tas  union_tas = "3 4 10 8 11 19 15 14 9 12 18 34 23 27 29 25 26 39 37 32 16 18 36 40 35 28 ")

let graphique_construction_tas_tab () =
  creer_graphique_test_fonc (constructionTab) "constructionTab.plt" ;;
let graphique_ajoutIteratifs_tas_tab () =
  creer_graphique_test_fonc ajoutIteratifsTab "ajoutIteratifsTab.plt" ;;


Random.init 2 ;;

let test_arbre () =
  let arbre_0 = creer_arbre_binomial 0 in
  let arbre_10 = creer_arbre_binomial 10 in
  assert(nb_noeud arbre_0 = int_of_float (2.0 ** 0.0));
  assert(nb_noeud arbre_10 = int_of_float (2.0 ** 10.0))

let test_file () =
  let taille = 200 in
  let max = 100 in
  let liste_cle = List.init taille (fun i -> int32_tuple_of_int (rand_int max)) in
  let ma_file = construction_fb liste_cle
in 
  List.iter (fun el ->  assert(est_tournoi_binomial el)) ma_file   ; 
  (* test à réalisé : voire dans quel ordre est H::T, est-ce que degre H > degre T ? Si oui, alors problématique, car on est censé partir de la fin *)
  (match ma_file with
  | h1::h2::t -> Printf.printf "degre H1 : %d , degre H2 %d \n" (deg h1) (deg h2)
  | _ -> print_string "lol")
  ;
    (* exit(1);oi *)
  let croissant = file_en_liste_croissante ma_file in (* true *)
  Printf.printf "taille croissant %d\n" (List.length croissant);
  begin match croissant with
  | el::queue -> (let _ = List.fold_left (fun acc el -> assert(inf acc el || eg el acc); el ) el croissant in true)
  | [] -> true
  end
   
   
;;
