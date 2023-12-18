(* 'a' -> false, '1' -> true *)
let isDigit (c : char) : bool =
  let i = int_of_char c in
  let zero = int_of_char '0' in
  let nine = int_of_char '9' in
  zero <= i && i <= nine

(* ['a', '1', 'j', '3' ] -> ['1', '3' ] *)
let filterDigits = List.filter isDigit

(* '1' -> 1 *)
let intFromChar (c : char) : int = int_of_char c - int_of_char '0'

(*
   Output has always two digits
   ['1'] -> 11
   ['1'; '5'] -> 15
   ['1'; '5'; '3'; '7'] -> 17
*)
let intFromCharList (cl : char list) : int =
  match cl with
  | [] -> 0
  | c :: [] ->
      let i = intFromChar c in
      (i * 10) + i
  | _ ->
      let first = List.nth cl 0 |> intFromChar in
      let last = List.nth cl (List.length cl - 1) |> intFromChar in
      (first * 10) + last

(* string -> string -> int -> string , "qsjoneqskl" -> "one" -> "1" -> "qsj1qskl" *)
(* string -> char list *)
let substituteString (str : string) =
  let rec substitute = function
    | [] -> []
    | 'o' :: 'n' :: 'e' :: xl -> '1' :: substitute xl
    | 't' :: 'w' :: 'o' :: xl -> '2' :: substitute xl
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: xl -> '3' :: substitute xl
    | 'f' :: 'o' :: 'u' :: 'r' :: xl -> '4' :: substitute xl
    | 'f' :: 'i' :: 'v' :: 'e' :: xl -> '5' :: substitute xl
    | 's' :: 'i' :: 'x' :: xl -> '6' :: substitute xl
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: xl -> '7' :: substitute xl
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: xl -> '8' :: substitute xl
    | 'n' :: 'i' :: 'n' :: 'e' :: xl -> '9' :: substitute xl
    | x :: xl -> x :: substitute xl
  in
  let cl = str |> String.to_seq |> List.of_seq in
  let scl = substitute cl in
  scl |> List.to_seq |> String.of_seq

let intFromString (str : string) : int =
  str |> String.to_seq |> List.of_seq |> filterDigits |> intFromCharList

let part1 ls =
  ls |> List.map intFromString |> List.fold_left ( + ) 0 |> string_of_int

let part2 ls =
  let sls = List.map substituteString ls in
  part1 sls
