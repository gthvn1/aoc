(** [string_from_input string] returns a list of strings [l]. A new
    string is created for each new line. *)
let string_from_input filename =
  try
    let channel = open_in filename in
    try
      let rec read_lines lines =
        try
          let line = input_line channel in
          read_lines (line :: lines)
        with End_of_file -> List.rev lines
      in
      let lines = read_lines [] in
      close_in channel;
      lines
    with ex ->
      close_in channel;
      raise ex
  with Sys_error msg ->
    prerr_endline ("Cannot open file: " ^ msg);
    exit 1
