open Cle
open Tas
open Graphe


let () = 
  Test.test_tas_min () ;;

Test.test_arbre ();;            (* test arbre binomial *)
Test.test_file ();;             (* file binomiale *)
  (* Test.graphique_construction_tas_tab () ;;
   * Test.graphique_ajoutIteratifs_tas_tab () ;; *)

