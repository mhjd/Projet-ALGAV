open Cle
open Tas
open Graphe

let () = 
  creer_graphique_test_fonc (constructionTab) "constructionTab.plt" ;;

(* creer_graphique_test_fonc ajoutIteratifsTab "ajoutIteratifsTab.plt" ;; *)

(* let test_ajout = *)
(*   (\* Création d'un tas *\) *)
(*   let tas_initial : tas_min_tab =  ajoutIteratifsTab [(conv (0,0,0,2)) ; (conv (0,0,0,6)) ; (conv (0,0,0,5)) ; (conv (0,0,0,10)) ; (conv (0,0,0,13)) ; (conv (0,0,0,7)) ; (conv (0,0,0,8)) ; (conv (0,0,0,12)) ; (conv (0,0,0,15)) ; (conv (0,0,0,14))] 0 in *)

(*   (\* Affichage du tas initial *\) *)
(*   afficher_tas tas_initial *)

(* let test_suppr = *)
(*   (\* Création d'un tas *\) *)
(*   let tas_initial : tas_min_tab =  ajoutIteratifsTab [(conv (0,0,0,2)) ; (conv (0,0,0,6)) ; (conv (0,0,0,5)) ; (conv (0,0,0,10)) ; (conv (0,0,0,13)) ; (conv (0,0,0,7)) ; (conv (0,0,0,8)) ; (conv (0,0,0,12)) ; (conv (0,0,0,15)) ; (conv (0,0,0,14))] 0 in *)

(*   (\* Affichage du tas initial *\) *)
(*   afficher_tas tas_initial; *)

(*   (\* Suppression du minimum *\) *)
(*   let min_supprime = supprMinTab tas_initial in *)
(*   (\* Affichage du tas après suppression *\) *)
(*   afficher_tas tas_initial; *)
(*   (\* Affichage de la valeur supprimée *\) *)
(*   Printf.printf "Valeur supprimée : %d\n" (match min_supprime with Some (_, _, _, x) -> Int32.to_int x | None -> 0) *)

