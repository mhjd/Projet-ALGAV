open Cle

type abr =
| Noeud of cle * abr * abr
| Vide

let est_arbre_vide (abr:abr) =
  match abr with
  | Vide -> true
  | _ -> false
let racine (abr:abr) =
  match abr with
  | Noeud(cle, _, _) -> cle
  | Vide -> failwith "pas de racine à un arbre vide"
let sag (abr:abr) =
  match abr with
  | Noeud(_, le_sag, _) -> le_sag
  | _ -> failwith "pas de sag"

let sad (abr:abr) =
  match abr with
  | Noeud(_, _, le_sad) -> le_sad
  | _ -> failwith "pas de sad"

let rec ajout_abr (abr:abr) (el:cle) : abr =
  if est_arbre_vide abr then
    Noeud(el, Vide, Vide)
  else if eg el (racine abr) then
    abr
  else if inf el (racine abr) then
    Noeud(racine abr, ajout_abr (sag abr) el, (sad abr))
  else
    Noeud( racine abr, sag abr, ajout_abr (sad abr) el)

let rec recherche (abr:abr) (el:cle) : bool =
  match abr with
  | Noeud(cle, le_sag, le_sad) -> if eg cle el then true
                                  else if inf cle el then recherche le_sag cle
                                  else recherche le_sad cle
  | Vide -> false 
