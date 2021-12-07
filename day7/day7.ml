open Base;;
open Stdio;;
open Kock;;

let abs_sum = List.fold ~f:(fun acc b -> acc + abs b) ~init:0 

let int_sum_to n = (n * (n + 1)) / 2

let fuel_for pos = List.fold ~f:(fun acc crab -> (+) acc @@ int_sum_to @@ abs @@ crab - pos) ~init:0

let part2 filename = 
    In_channel.create filename 
    |> IO.stdin_map (IO.int_line ',')
    |> List.hd_exn 
    |> (fun crabs -> 
        List.map ~f:(
            fun i -> (i, fuel_for i crabs)) (0--(Lst.max_exn Int.compare crabs)))
    |> Lst.min_exn (fun (_, x) (_, y) -> Int.compare x y ) 
    |> snd
    |> printf "%d\n"

let part1 filename = 
    In_channel.create filename 
    |> IO.stdin_map (IO.int_line ',')
    |> List.hd_exn 
    |> List.sort ~compare:Int.compare
    |> fun sorted -> 
        let l = List.length sorted in 
        let d = List.nth_exn sorted (l / 2) in
        abs_sum (List.map ~f:(fun a -> a - d) sorted) 
    |> printf "%d\n"

let () = 
    let filename = "day7/input.txt" in 
    part1 filename; part2 filename
