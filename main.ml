open Cle
open Tas
open Graphe

let test_union_tas_tab constructionTab =
  let tas1 = (constructionTab [conv (0, 0, 0, 37);  conv (0, 0, 0, 18); conv (0, 0, 0, 15); conv (0, 0, 0, 39); conv (0, 0, 0, 11); conv (0, 0, 0, 40); conv (0, 0, 0, 27); conv (0, 0, 0, 25); conv (0, 0, 0, 9); conv (0, 0, 0, 32); conv (0, 0, 0, 3); conv (0, 0, 0, 19); conv (0, 0, 0, 23); conv (0, 0, 0, 10); conv (0, 0, 0, 29); conv (0, 0, 0, 14); conv (0, 0, 0, 26); conv (0, 0, 0, 4)] 0) in

  let tas2 = constructionTab [conv (0, 0, 0, 18); conv (0, 0, 0, 28); conv (0, 0, 0, 35); conv (0, 0, 0, 8); conv (0, 0, 0, 36); conv (0, 0, 0, 34); conv (0, 0, 0, 16); conv (0, 0, 0, 12)] 0 in

  Printf.printf "Tas 1 avant l'union : ";
  afficher_tas tas1;
  Printf.printf "Taille : %d\n" !(snd tas1);

  Printf.printf "Tas 2 avant l'union : ";
  afficher_tas tas2;
  Printf.printf "Taille : %d\n" !(snd tas2);

  let tas_union = union_tas_tab tas1 tas2 in

  Printf.printf "Tas résultant de l'union : ";
  afficher_tas (tas_union);

  Printf.printf "Taille attendue de l'union : %d\n" (!(snd tas1) + !(snd tas2));

  Printf.printf "Contenu attendu de l'union : ";
  Array.iter (fun elem ->
    match elem with
    | Some x -> Printf.printf "%d " (val_cle x)
    | None -> Printf.printf "%d " 0
  ) !(fst tas_union);
  print_newline ()
;;

let () = 
  creer_graphique_test_fonc (constructionTab) "constructionTab.plt" ;;
creer_graphique_test_fonc ajoutIteratifsTab "ajoutIteratifsTab.plt" ;;

