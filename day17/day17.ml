open Base;;
open Kock;;
open Stdio;;

let str_drop n line = 
    String.to_list line 
    |> (Fn.flip List.drop) n
    |> String.of_char_list

let to_pair = function 
        [a; b] -> (a, b)
        | _ -> raise @@ Invalid_argument "the list is not tuple"

let parse_range line = 
    Str.split (Str.regexp "[.][.]") line
    |> List.map ~f:(Int.of_string)
    |> to_pair

let parse_input line = 
    String.chop_prefix_exn ~prefix:"target area: " line
    |> Str.split (Str.regexp ", ") 
    |> List.map ~f:(parse_range % str_drop 2)
    |> to_pair

let read_input filename = 
    In_channel.create filename
    |> IO.stdin_map parse_input
    |> List.hd_exn

let arithmetic n = n * (n + 1) / 2

let part1 ((_, _), (y, yy)) = 
    (if y > yy then y else abs(y) - 1)
    |> arithmetic
    |> printf "%d\n" 

type ball = { x : int ; y : int ; vx : int ; vy : int }
let tick b = { x = b.x + b.vx ; y = b.y + b.vy ; vx = max 0 (b.vx - 1) ; vy = b.vy - 1 }

let get_extremes ((_, xx), (y, _) : (int * int) * (int * int)) = xx, abs(y) - 1

let test_inter b ((x, xx), (y, yy)) = x <= b.x && b.x <= xx && y <= b.y && b.y <= yy

let rec test_ball_step ball ((x, xx), (y, yy)) =
    if test_inter ball ((x, xx), (y, yy)) 
        then true
        else if ball.y < y || ball.x > xx
            then false 
            else test_ball_step (tick ball) ((x, xx), (y, yy))


let test_ball b vx vy = test_ball_step { x = 0; y = 0; vx = vx; vy = vy; } b

let part2 b =
    let ex, ey = get_extremes b in
    List.filter ~f:(fun (vx, vy) -> test_ball b vx vy) ((0, ex + 1) --- (-ey - 1, ey + 1))
    |> use_pass @@  Lst.print (fun (vx, vy) -> printf "%d %d" vx vy)
    |> List.length
    |> printf "%d\n"

let () = 
    let parsed = read_input "day17/input.txt" in 
    part1 parsed;
    part2 parsed
