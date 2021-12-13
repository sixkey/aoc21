open Kock;;
open Base;;
open Stdio;;
open Poly;;

module Point = struct 
    type t = (int * int) [@@deriving compare, sexp_of]
end

module PointComparator = struct
    include Point
    include Comparator.Make(Point)
end

type direction = Hor | Ver

let parse_pair acc line = 
    String.split ~on:',' line
    |> function
        | [f;sd] -> Some ((Int.of_string f, Int.of_string sd)::acc)
        | _ -> None

let parse_fold line = 
    String.chop_prefix_exn ~prefix:"fold along " line 
    |> String.split ~on:'=' 
    |> function 
        | [f;sd] -> ((if f = "x" then Hor else Ver), Int.of_string sd)
        | _ -> raise (Invalid_argument "the line needs to have = in it") 

let read_input filename =
    In_channel.create filename 
    |> fun stdin -> 
        IO.stdin_foldi_while (fun _ -> parse_pair) [] stdin
        |> fst 
        |> Set.of_list (module PointComparator)
        |> fun coords -> 
            let folds = List.rev @@ IO.stdin_map (parse_fold) stdin in
            coords, folds

type point_set = (PointComparator.t, PointComparator.comparator_witness) Set.t

let get_fold = function 
    | (Hor, h) -> fun (x, y) -> if x < h then (x, y) else ((h - x % h) % h, y)
    | (Ver, v) -> fun (x, y) -> if y < v then (x, y) else (x, (v - y % v) % v)

let apply_fold (point_set : point_set) (fold : direction * int) = 
    Set.map (module PointComparator) ~f:(get_fold fold) point_set

let matrix_of_pointset point_set = 
    let (maxx, maxy) = Set.fold ~f:(fun (mx, my) (x, y) -> (max mx x, max my y)) ~init:(0, 0) point_set in
    let matrix = Matrix.make (maxx + 1) (maxy + 1) false in 
    Set.iter ~f:(fun (x, y) -> matrix.m.(x).(y) <- true) point_set; 
    matrix

let draw_matrix =  
    Matrix.print (fun (v) -> if v then printf "#" else printf ".") 

let part1 filename = 
    let point_set, folds = read_input filename in 
    apply_fold point_set (List.hd_exn folds)
    |> Set.length
    |> printf "%d\n"


let part2 filename = 
    let point_set, folds = read_input filename in 
    List.fold ~f:apply_fold ~init:point_set folds 
    |> matrix_of_pointset 
    |> draw_matrix

let () = 
    let filename = "day13/input.txt" in 
    part1 filename; part2 filename
