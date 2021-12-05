open Kock;;
open Base;; 
open Stdio;;


module Point2D = struct
    type t = {x : int; y: int}

    let of_list = function 
        | hd::md::_ -> Some {x = hd; y = md}
        | _ -> None 

    let of_tuple (a, b) = {x = a; y = b}

    let print {x; y} = printf "[%d, %d]" x y

    let parse (s : string) : t option = 
        String.split_on_chars ~on:[','] s
        |> List.map ~f:Int.of_string
        |> of_list 
end

module Line2D = struct
    type t = {a : Point2D.t; b : Point2D.t }

    let of_list = function
        | hd::md::_ -> Some {a = hd; b = md}
        | _ -> None

    let print {a; b} = Point2D.print a ; print_string " → "; Point2D.print b

    let parse (line : string) : t option = 
        let r = Str.regexp " -> " in 
        Str.split r line 
        |> List.filter_map ~f:Point2D.parse
        |> of_list

    let to_list {a; b} = 
        let minx, maxx = min a.x b.x, max a.x b.x in 
            let miny, maxy = min a.y b.y, max a.y b.y in 
        if a.x = b.x || a.y = b.y then 
            List.map ~f:Point2D.of_tuple @@ (minx, maxx + 1) --- (miny, maxy + 1) 
        else if abs (a.x - b.x) = abs (a.y - b.y) then 
            let l, r = if a.x < b.x then a, b else b, a in
            let c = (r.y - l.y) / (r.x - l.x) in
            List.init (r.x - l.x + 1) ~f:(fun i -> Point2D.of_tuple (l.x + i, l.y + c * i))
        else []
end

type object2D = Line of Line2D.t | Point of Point2D.t

module Rect2D = struct 
    type t = {l : int; t: int; r: int; b: int}

    let of_tuple (l, t, r, b) = {l = l; t = t; r = r; b = b}

    let rec expand (o : object2D) (r : t) = match o with
        | Line {a; b} -> expand (Point a) r |> expand (Point b)
        | Point p -> (min r.l p.x, min r.t p.y, max r.r p.x, max r.b p.y) |> of_tuple

    let print {l; t; r; b} = printf "%d, %d, %d, %d" l t r b 
end 

module Matrix = struct
    type t = {m : int array array; w : int; h : int } 

    let make w h v = let m = Array.make_matrix ~dimx:w ~dimy:h v in { m = m ; w = w ; h = h }
    
    let print {m; w; h} = 
        for y = 0 to h - 1 do 
            for x = 0 to w - 1 do 
                printf (if x <> 0 then ", %d" else "%d") m.(x).(y)
            done;
            printf "\n" 
        done

    let rec fill m = function 
        | Point {x; y} -> m.m.(x).(y) <- m.m.(x).(y) + 1; m
        | Line l -> List.fold ~f:(fill) ~init:m @@ List.map ~f:(fun x -> Point x) @@ Line2D.to_list l

    let to_listi {m; w; h} = List.fold ~f:(fun acc (x, y) -> (x, y, m.(x).(y)) :: acc) ~init:[] ((0, w) --- (0, h))
end

let m_fill_lines (m : Matrix.t) (lines : Line2D.t list) : Matrix.t = 
    List.fold ~f:(Matrix.fill) ~init:m (List.map ~f:(fun x -> Line x) lines)

let parse_input filename = 
    In_channel.create filename
    |> IO.stdin_filter_map Line2D.parse 

let lines_in_matrix lines = 
    List.fold ~f:(fun r l -> Rect2D.expand (Line l) r) ~init:(Rect2D.of_tuple (0, 0, 0, 0)) lines
    |> fun rect -> 
        let m = Matrix.make (rect.r - rect.l + 1) (rect.b - rect.t + 1) 0
        in m_fill_lines m lines 

let part1 filename = 
    parse_input filename
    |> List.filter ~f:(fun Line2D.{a; b} -> a.x = b.x || a.y = b.y) 
    |> lines_in_matrix
    |> Matrix.to_listi 
    |> List.count ~f:(fun (_, _, v) -> v >= 2) 
    |> printf "%d\n"

let part2 filename = 
    parse_input filename
    |> lines_in_matrix
    |> Matrix.to_listi 
    |> List.count ~f:(fun (_, _, v) -> v >= 2) 
    |> printf "%d\n"

let () = 
    let filename = "day5/input.txt" in 
    part1 filename; part2 filename
