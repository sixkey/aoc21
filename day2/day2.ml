open Format

let read_line stdin = 
    try Some (input_line stdin) with 
        End_of_file -> None;;

let rec stdin_fold tran start stdin = match read_line stdin with
        Some line -> stdin_fold tran (tran start line) stdin
    |   None -> start

let parse_line line = String.split_on_char(' ') line |> function 
    | dir::loc::_ -> dir, int_of_string loc
    | _ -> "", 0

let move (pos, depth) (dir, loc) =
    match dir with 
        "forward" -> pos + loc, depth
    |   "up" -> pos, depth - loc
    |   "down" -> pos, depth + loc
    |   _ -> pos, depth

let move2 (pos, depth, aim) (dir, x) =
    match dir with 
        "forward" -> pos + x, depth + aim * x, aim
    |   "up" -> pos, depth, aim - x
    |   "down" -> pos, depth, aim + x
    |   _ -> pos, depth, aim

let processor pos line = move2 pos (parse_line line)

let () = open_in "input.txt" |> stdin_fold processor (0, 0, 0) 
        |> function p, d, _ -> print_int (p * d)

