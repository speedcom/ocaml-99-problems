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

(* Problem 4, without tail recursion *)
let rec length = function
  | []     -> 0;
  | h :: t -> 1 + length t
;;

(* Problem 4, with tail recursion *)
let length list =
  let rec inner_length buf = function
    | []     -> buf;
    | _ :: t -> inner_length (buf+1) t
  in inner_length 0 list
;;

(* Problem 5 *)
let rev list =
  let rec inner_rev reverted = function
    | []     -> reverted
    | h :: t -> inner_rev (h :: reverted) t
  in inner_rev [] list
;;

(* Problem 6 *)
let is_palindrome list = list = rev(list) ;;

(* Problem 7 *)
type 'a node =
  | One of 'a
  | Many of 'a node list
;;

let flatten list =
  let rec inner_flatten result = function
    | []            -> result
    | One  hd :: tl -> inner_flatten (hd :: result) tl
    | Many hd :: tl -> inner_flatten (inner_flatten result hd) tl
  in List.rev (inner_flatten [] list)
;;

(* Problem 8, first version - that's wrong because it check global existence *)
let rec find_opt p list =
  match list with
  | []       -> None
  | hd :: tl -> if hd = p then Some hd else find_opt p tl
;;

let compress list =
  let rec inner_compress result = function
    | []       -> result
    | hd :: tl -> if (find_opt hd result) = None then inner_compress (hd :: result) tl else inner_compress result tl
  in List.rev (inner_compress [] list)
;;

(* Problem 8, second version *)
let rec compress = function
  | (a :: (b :: tl)) -> if a = b then compress (b :: tl) else a :: compress (b :: tl)
  | result           -> result
;;

(* Problem 8, third version *)
let rec compress = function
  | (a :: (b :: _ as tl)) -> if a = b then compress tl else a :: compress tl
  | result                -> result
;;

(* Problem 14 *)
let duplicate list =
  let rec inner_duplicate result = function
  | []       -> result
  | hd :: tl -> inner_duplicate (hd :: hd :: result) tl
  in List.rev (inner_duplicate [] list)
;;

(* Problem 15 *)
let replicate list times =
  let rec inner_replicate result = function
  | []       -> result
  | hd :: tl ->
    let rec multiple el = function
      | t when t > 0 -> el :: (multiple el (t-1))
      | _            -> []
    in
    inner_replicate ((multiple hd times) :: result) tl
  in List.rev (List.flatten (inner_replicate [] list))
;;

(* Problem 16 *)
let drop list n =
  let rec inner_drop list m =
    match list with
    | []       -> []
    | hd :: tl -> if(m == 1) then inner_drop tl n else hd :: inner_drop tl (m-1)
  in inner_drop list n
;;