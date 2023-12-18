type cubes = { red : int; green : int; blue : int }
type games = cubes list

(** [colorOfString str -> str -> int] return number of cubes for a given color
    "red" "4 red" -> 4
    "red" "5 blue" -> 0 *)
let colorOfString (str_color : string) (str : string) : int =
  let ls = String.split_on_char ' ' str in
  let value = List.nth ls 0 in
  let color = List.nth ls 1 in
  if color = str_color then int_of_string value else 0

(** [cubesOfString str -> cubes] take a set of cubes and return a cubes.
    "3 blue, 4 red" -> {red: 4; green: 0; blue: 3} *)
let cubesOfString (str : string) : cubes =
  let lc = String.split_on_char ',' str |> List.map String.trim in
  (* list of colors ["3 blue", "4 red"] *)
  let r = List.map (colorOfString "red") lc |> List.fold_left ( + ) 0 in
  let g = List.map (colorOfString "green") lc |> List.fold_left ( + ) 0 in
  let b = List.map (colorOfString "blue") lc |> List.fold_left ( + ) 0 in
  { red = r; green = g; blue = b }

let gameIsValid ~(r : int) ~(g : int) ~(b : int) (c : cubes) : bool =
  c.red <= r && c.green <= g && c.blue <= b

(** [gameOfString str -> game] take a string and return true if the game
    is valid.
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> true
    "Game 1: 63 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> false *)
let gameOfStringIsValid (str : string) : bool =
  let cubes_str = List.nth (String.split_on_char ':' str) 1 |> String.trim in
  let lc =
    String.split_on_char ';' cubes_str
    |> List.map String.trim |> List.map cubesOfString
  in
  (* List of cubes with index *)
  let lcf = List.map (fun g -> gameIsValid ~r:12 ~g:13 ~b:14 g) lc in
  List.fold_left ( && ) true lcf

(** [minSetCubesOfString str] return  the minimum set that is required
    to play the game. For example:
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" ->
    {red = 4, green = 2, blue = 6} *)
let minSetCubesOfString (str : string) : cubes =
  let cubes_str = List.nth (String.split_on_char ':' str) 1 |> String.trim in
  String.split_on_char ';' cubes_str
  |> List.map String.trim |> List.map cubesOfString
  |> List.fold_left
       (fun (a : cubes) (b : cubes) ->
         {
           red = max a.red b.red;
           green = max a.green b.green;
           blue = max a.blue b.blue;
         })
       { red = 0; green = 0; blue = 0 }

let powerOfCubes (c : cubes) : int = c.red * c.green * c.blue

let part1 ls =
  List.map gameOfStringIsValid ls
  |> List.mapi (fun idx v -> (idx + 1, v))
  |> List.filter (fun v -> snd v)
  |> List.fold_left (fun a b -> a + fst b) 0
  |> string_of_int

let part2 ls =
  List.map minSetCubesOfString ls
  |> List.map powerOfCubes |> List.fold_left ( + ) 0 |> string_of_int
