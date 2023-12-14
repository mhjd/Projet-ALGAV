open Cle
open Structure_de_donnee
open Tas
open File
open Graphe

     
let rand_int n = Random.int n

let int32_tuple_of_int x =
  (Int32.zero, Int32.zero, Int32.zero, Int32.of_int x)
let conv = int32_tuple_of_int ;;

let graphique_construction_tas_tab () =
  Graphe_tas_min.creer_graphique_construction "constructionTab.plt" ;;
let graphique_ajoutIteratifs_tas_tab () =
  Graphe_tas_min.creer_graphique_ajouts_iteratifs "ajoutIteratifsTab.plt" ;;

Random.init 2 ;;

let test_arbre () =
  let arbre_0 = creer_arbre_binomial 0 in
  let arbre_10 = creer_arbre_binomial 10 in
  assert(nb_noeud arbre_0 = int_of_float (2.0 ** 0.0));
  assert(nb_noeud arbre_10 = int_of_float (2.0 ** 10.0))

let test_struct (module Ma_structure : Data_structure) =
  (* créations des clés *)
  let taille = 200 in
  let max = 100 in
  (* let liste_cle = List.init taille (fun i -> int32_tuple_of_int (rand_int max)) in *)
  (* let liste_cle = [conv 37;  conv 18; conv 15; conv 39; conv 11; conv 40; conv 27; conv 25; conv 9; conv 32; conv 3; conv 19; conv 23; conv 10; conv 29; conv 14; conv 26; conv 4] in *)

  let liste_cle = [conv 6 ; conv 5  ; conv 2 ; conv 10 ; conv 13 ; conv 7 ; conv 8 ; conv 12 ; conv 15 ; conv 14] in

  let ma_struct = Ma_structure.ajouts_iteratifs liste_cle in
 (* let ma_struct = Ma_structure.construction liste_cle in  *)
  let croissant: cle list = Ma_structure.en_liste_croissante ma_struct in 
  Printf.printf "taille croissant %d\n" (List.length croissant);
  begin match croissant with
  | el::queue -> (List.fold_left (fun acc el -> Printf.printf "el : %d\n" (val_cle el) ; assert(inf acc el || eg el acc); el ) el croissant; ())
  | [] -> ()
  end


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
