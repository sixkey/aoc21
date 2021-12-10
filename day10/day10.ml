open Base;;
open Stdio;;
open Kock;;
open Base.Poly;;


let get_score (a : char) : int = match a with 
    | '(' -> 3 
    | ')' -> 3 
    | '[' -> 57
    | ']' -> 57
    | '{' -> 1197
    | '}' -> 1197
    | '<' -> 25137
    | '>' -> 25137
    | _ -> 0 

let auto_score (a : char) : int = match a with 
    | ')' -> 1 
    | ']' -> 2 
    | '}' -> 3 
    | '>' -> 4
    | _ -> 0 

let is_opening (a : char) : bool = match a with 
    | '(' -> true 
    | '[' -> true
    | '{' -> true
    | '<' -> true
    | _ -> false 

let get_match (a : char) : char = match a with 
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | n -> n 

let is_match (a : char) (b : char) : bool = get_match a = b

let rec fold_line (line : char list) (stack : char list) = match line with 
    | [] -> Ok stack 
    | hd::tl -> if is_opening hd then fold_line tl (hd::stack) else 
        match stack with 
            | [] -> Error hd 
            | shd::stl -> if is_match shd hd then fold_line tl stl else Error hd

let score_line (line : string) : int = match fold_line (String.to_list line) [] with 
    | Ok _ -> 0
    | Error c -> get_score c

let repair_line (line : string) : int option = match fold_line (String.to_list line) [] with 
    | Ok stack -> Some (List.fold ~f:(fun acc v -> acc * 5 + (get_match v |> auto_score)) ~init:0 stack)
    | Error _ -> None

let part1 filename = 
    In_channel.create filename
    |> IO.stdin_map score_line
    |> List.sum (module Int) ~f:id
    |> printf "%d\n"

let part2 filename =
    In_channel.create filename
    |> IO.stdin_filter_map repair_line
    |> List.sort ~compare:Int.compare
    |> fun (lines) -> 
        let n = List.length lines in 
        printf "%d\n" @@ List.nth_exn lines (n / 2)

let () = 
    let filename = "day10/input.txt" in 
    part1 filename; part2 filename
