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

(* Problem 3, first version *)
let rec at idx list =
    match list with
    | []                       -> None
    | el :: _  when idx == 1   -> Some el
    | _ :: tail                -> at (idx-1) tail
  ;;

(* Problem 3, second version *)
let rec at idx = function
    | []     -> None
    | h :: t -> if idx = 1 then Some h else at (idx-1) t
  ;;
