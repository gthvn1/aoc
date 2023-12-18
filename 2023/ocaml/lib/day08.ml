(* We will store the navigation map as: "AAA" -> ("BBB", "CCC")...
   So the key of the map will be the string and corresponding
   element will be a tuple with (<left>, <right>) where left and
   right are also strings. *)
module NavMap = Map.Make (String)

(** [navigate_to_or_end src dst navmap path] returns the number of steps required to
    reach [dst] following [path] on the given map [navmap] starting from [src].
    If [dst] cannot be reached then it returns the number of steps to reach the
    destination at the end of the path. *)
let navigate_to_or_end ~(src : string) ~(dst : string)
    ~(navmap : (string * string) NavMap.t) ~(path : string) : int * string =
  let p = String.to_seq path |> List.of_seq in
  let rec aux (k : string) (p : char list) (s : int) : int * string =
    if String.ends_with k ~suffix:dst then (s, dst)
    else
      match p with
      | [] -> (s, k)
      | direction :: xd ->
          let left, right = NavMap.find k navmap in
          if direction = 'L' then aux left xd (s + 1) else aux right xd (s + 1)
  in
  aux src p 0

let all_end_with_z = List.for_all (fun str -> String.ends_with ~suffix:"Z" str)

(** navigate on multiple path *)
let rec navigate_sim ~navmap ~(start_list : string list) ~(path : char list)
    ~(steps : int) : string list * int =
  match path with
  | [] -> (start_list, steps)
  | dir :: xd ->
      let go_direction node =
        let left, right = NavMap.find node navmap in
        if dir = 'L' then left else right
      in
      let new_pos = List.map go_direction start_list in
      if all_end_with_z new_pos then (new_pos, steps + 1)
      else navigate_sim ~navmap ~start_list:new_pos ~path:xd ~steps:(steps + 1)

(** [get_shortcut_to_or_end dst navmap path] returns for each key of the navmap the destination and the number of
      steps to reach [dst] or the end of path. *)
let get_shortcut_to_or_end (dst : string) (navmap : (string * string) NavMap.t)
    (path : string) : (int * string) NavMap.t =
  NavMap.mapi (fun src _ -> navigate_to_or_end ~src ~dst ~navmap ~path) navmap

(** [get_shortcut_to src dst navmap path] returns for the given [src] the number
    of steps to reach [dst] by following the [path] of the [navmap].
    We suppose that destination can be reached otherwise we will loop infinity. *)
let get_shortcut_to (src : string) (dst : string)
    (navmap : (string * string) NavMap.t) (path : string) : int =
  let rec aux s steps =
    let steps', dst' = navigate_to_or_end ~src:s ~dst ~navmap ~path in
    if String.ends_with ~suffix:dst dst' then steps + steps'
    else aux dst' (steps + steps')
  in
  aux src 0

(** [keyval_of_string s] returns a tuple of key, value where key is a string and
    value is a tuple of two strings. *)
let keyval_of_string (s : string) : string * (string * string) =
  let items = String.split_on_char '=' s in
  let key = List.nth items 0 |> String.trim in
  let values =
    List.nth items 1 |> String.split_on_char ',' |> List.map String.trim
  in
  (*at this point first value has an open parenthesis and second value has
     a close parenthesis. *)
  let left = String.sub (List.nth values 0) 1 3 in
  let right = String.sub (List.nth values 1) 0 3 in
  (key, (left, right))

(** [parse_input input] returns a map of navigation by parsing the list
    of strings found in [input]. String must be a valid parameter to
    keyval_of_string *)
let parse_input input : (string * string) NavMap.t =
  List.fold_left
    (fun acc s ->
      let k, v = keyval_of_string s in
      NavMap.add k v acc)
    NavMap.empty input

let sample =
  [ "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)" ]

let sample2 =
  [
    "LR";
    "";
    "11A = (11B, XXX)";
    "11B = (XXX, 11Z)";
    "11Z = (11B, XXX)";
    "22A = (22B, XXX)";
    "22B = (22C, 22C)";
    "22C = (22Z, 22Z)";
    "22Z = (22B, 22B)";
    "XXX = (XXX, XXX)";
  ]

let part1 ls =
  let path = List.hd ls in
  let main_map = parse_input (List.tl ls |> List.tl) in
  let shortcuts = get_shortcut_to_or_end "ZZZ" main_map path in
  let rec aux (start : string) (steps : int) : int =
    match NavMap.find start shortcuts with
    | s, "ZZZ" -> steps + s
    | s, dest -> aux dest (steps + s)
  in
  aux "AAA" 0 |> string_of_int

let part2 ls =
  let path = List.hd ls in
  let main_map = parse_input (List.tl ls |> List.tl) in
  let initial_list =
    NavMap.fold
      (fun key _ acc ->
        if String.ends_with ~suffix:"A" key then key :: acc else acc)
      main_map []
  in
  let rec pgcd a b =
    let diviseur = min a b in
    let dividende = max a b in
    let reste = dividende mod diviseur in
    if reste = 0 then diviseur else pgcd diviseur reste
  in
  let ppcm a b = a * b / pgcd a b in
  let steps =
    List.map (fun src -> get_shortcut_to src "Z" main_map path) initial_list
  in
  List.fold_left ppcm (List.hd steps) (List.tl steps) |> string_of_int
(* Compute the ppcm of all steps because if you look at the final node the
   next one is the same than the node of step1. So interval between two
   nodes that end with "Z" for a given start node is always the same. Thus
   they will all reach "Z" at PPCM !!!
   -- START NODES
   LQA = (VQD, QKX)  <- VQD QKX
   SGA = (XGL, QVP)
   AAA = (FCS, HHF)
   BJA = (CBD, MDP)
   SVA = (FXQ, KJJ)
   GFA = (VNG, GGP)
   -- END NODES
   FBZ = (QKX, VQD)  <- Oh oh oh... QKX VQD, that's why ppcm is working :)
   QNZ = (QVP, XGL)
   ZZZ = (HHF, FCS)
   QXZ = (MDP, CBD)
   LHZ = (KJJ, FXQ)
   BRZ = (GGP, VNG)
*)
