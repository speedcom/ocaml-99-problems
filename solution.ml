(* Problem 1 *)
let rec last list =
    match list with
    | []        -> None
    | [el]      -> Some el
    | _ :: tail -> last tail
  ;;

(* Problem 2 *)
let rec last_two list =
    match list with
    | []               -> None
    | el1 :: el2 :: [] -> Some (el1, el2)
    | hd :: tail       -> last_two tail
  ;;
