open Cle
open Tas

let string_en_cle (string_en_cle:string) =
  (
    (* 2 : c'est le 0x au début *)
  (* Printf.printf "Clé au départ : %s \n" string_en_cle ; *)
  (* il y a certain nombre hexadécimaux ayant moins de 32 chiffre, il faut donc ajouter des 0 devant : *)
  let string_en_string32 =
    (* "0x" + 32 octets = 34 octets *)
    if String.length string_en_cle < 34 then
      (
      let nb_caractere_en_moins = 34 - String.length string_en_cle in 
      let nb_caractere = (32 - nb_caractere_en_moins) in
      let nb_0 = String.make nb_caractere_en_moins '0' in
      (* print_string  nb_0 ; *)
       "0x" ^ nb_0  ^ String.sub string_en_cle 2 nb_caractere
      )
    else
      string_en_cle
  in
  (* Printf.printf "\n Ma chaine : %s  \n" string_en_string32 ; *)
  let mes_bytes_en_string = List.map (fun i -> (* print_int i ; *) String.sub string_en_string32 (2+i*8) 8) [0; 1; 2; 3] in
  (* print_string "Découpage en quatre morceaux : \n" ; *)
  (* List.iter (fun x -> Printf.printf "mon string : %s \n" x) mes_bytes_en_string ;  *)
  let mes_bytes_en_int32 = List.map (fun i -> Int32.of_string ("0x"^i)) mes_bytes_en_string in
  (* print_string "Transformation en int32 : \n" ; *)
  (* List.iter (fun x -> Printf.printf "mon string : %s \n" (Int32.to_string x)) mes_bytes_en_int32 ; *)
  match mes_bytes_en_int32 with
  | [el1; el2; el3; el4] -> (el1, el2, el3, el4)
  | [el2; el3; el4] -> (Int32.zero, el2, el3, el4)
  | [el3; el4] -> (Int32.zero, Int32.zero, el3, el4)
  | [el4] -> (Int32.zero, Int32.zero, Int32.zero, el4)
  | _ -> failwith "anormal, il est censé y avoir au moins un entier, et pas plus de 4"

  )
let fichier_vers_liste_cle (fichier:string):cle list =
  let channel = open_in fichier in
  let rec lire channel (liste_cle_str:cle list) =
    (
    try
      match input_line channel with
      | exception End_of_file -> liste_cle_str
      | el -> lire channel ((string_en_cle el)::liste_cle_str) 
    with e ->
      close_in_noerr channel ;
      raise e
    ) in lire channel []

let temps (fonc):float =
  (* failwith "chacal" ; *)
  let debut = Sys.time() in
  let _ = fonc in
  let fin = Sys.time() in
  fin -. debut

let moyenne_temps (fonc: (cle list -> int -> tas_min_tab)) (taille:int) =
  let noms_fichier:string list = List.map (fun i -> Printf.sprintf "./cles_alea/cles_alea/jeu_%d_nb_cles_%d.txt" i taille) [1; 2; 3; 4; 5] in
  (* liste contenant les listes de clés de chaque fichier *)
  let liste_cle_fichier:cle list list = List.map (fun fichier -> fichier_vers_liste_cle fichier) noms_fichier in
  let temps_cle:float list = List.map (fun cles_fichier -> temps (fonc cles_fichier taille))  liste_cle_fichier in
  let moyenne = (List.fold_left (+.) 0.0 temps_cle) /. 5.0 in
  moyenne


let tailles = [1000 ; 5000 ; 10000 ; 20000 ; 50000 ; 80000 ; 120000 ; 200000]

let liste_temps_tailles (fonc: (cle list -> int -> tas_min_tab)) =
  List.map (fun x -> (moyenne_temps fonc x, x)) tailles



let creer_graphique_test_fonc (fonc: (cle list -> int -> tas_min_tab)) (f_sortie:string) =
  let temps_moyens = liste_temps_tailles fonc in
  let file = open_out f_sortie in
  (* y a peut-être pas moyen de direct pattern matché en argument (temps, taille) *)

  List.iteri (fun i (temps, taille) ->  Printf.fprintf file "%d %f\n" taille temps) temps_moyens

;;
