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

(* Problem 9 *)
let pack list =
  let rec inner_pack current acc = function
  | []                 -> []
  | [x]                -> (x :: current) :: acc
  | (a :: (b :: _ as tl)) ->
    if a = b then
      inner_pack (a :: current) acc tl
    else
      inner_pack [] ((a :: current) :: acc) tl
  in List.rev (inner_pack [] [] list)
;;

(* Problem 10 *)
let encode list =
  let rec inner_encode result l =
    match (result, l) with
    | (_, [])                   -> result
    | ([], (a :: tl))           -> inner_encode ((1, a) :: result) tl
    | (((counter, el) :: rest), (a :: tl)) ->
      if (el = a) then
        inner_encode (((counter+1, el)) :: rest) tl
      else
        inner_encode ((1, a) :: result) tl
  in inner_encode [] (List.rev list)
;;

let encode2 list =
  List.map (fun el -> (List.length el, List.hd el)) (pack list)
;;

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

(* Problem 11 *)
let encode list =
  List.map (fun el -> if fst el = 1 then One (snd el) else Many (fst el, snd el)) (encode2 list)
;;

(* Problem 12 *)
let decode list =
  let rec multiple el time =
    if time > 0 then el :: multiple el (time -1)
    else []
  in
  let rec inner_decode result = function
    | []               -> result
    | One a :: t       -> inner_decode (a :: result) t
    | Many (c, a) :: t -> inner_decode ((multiple a c) @ result) t
  in inner_decode [] (List.rev list)
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

(* Problem 17 *)
let split list k =
  let list_length = List.length list
  in
  let rec inner_split result n = function
    | hd :: tl when n > 0 -> inner_split (hd :: (fst result), tl) (n-1) tl
    | _                   -> ((List.rev (fst result)), snd result)
  in
  if (k > list_length) then (list,[]) else inner_split ([],[]) k list
;;

(* Problem 18 *)
let slice list i k =
  let rec inner_slice result idx = function
    | []         -> result
    | (hd :: tl) ->
      if idx < i then inner_slice result (idx+1) tl
      else if idx >= i && idx <= k then inner_slice (hd :: result) (idx+1) tl
      else result
  in inner_slice [] (-1) (List.rev list)
;;

(* Problem 21 *)
let rec insert_at el idx = function
  | hd :: tl when idx > 0 -> hd :: insert_at el (idx-1) tl
  | hd :: tl as l         -> el :: l
  | _                     -> el :: []
;;

(* Problem 22 *)
let range i j =
  let rec inner_range a b result =
    if (a <= b) then inner_range (a+1) b (a :: result) else result

  in if(i < j) then List.rev(inner_range i j []) else inner_range j i []
;;

(* Problem 24 *)
let lotto n j =
  let rec inner_lotto result =
    if List.length result = n then
      result
    else
      let r = Random.int j
      in
      if(List.mem r result) then inner_lotto result else inner_lotto (r :: result)
  in inner_lotto []
;;

