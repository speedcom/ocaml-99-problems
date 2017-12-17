(* Problem 1 *)
let rec last list =
    match list with
    | []        -> None
    | [el]      -> Some el
    | _ :: tail -> last tail
  ;;
