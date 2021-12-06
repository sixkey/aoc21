open Base;;
open Stdio;;
open Kock;;

let start = [0];; 

let fish_tank () = Array.init 9 ~f:(fun _ -> 0)

let fill_tank (tank : int array) (fishes : int list) = 
    List.iter ~f:(fun f -> tank.(f) <- tank.(f) + 1) fishes; 
    tank

let sum_tank (tank : int array) = 
    Array.fold ~f:(fun acc v -> acc + v) ~init:0 tank

let step_fish (f : int) : int list = match f with
    | 0 -> [6; 8]
    | n -> [n - 1]

let fishes (tank : int array) (n: int) : int list = 
    let rec fish_step (tank : int array) (lst : int list) = 
    function 
        | 0 -> (tank, lst)
        | n -> 
            let res_tank = fish_tank () in 
            for i = 0 to 8 do 
                List.iter ~f:(fun v -> 
                    res_tank.(v) <- res_tank.(v) + tank.(i)) (step_fish i)
            done;
            fish_step res_tank (sum_tank res_tank :: lst) (n - 1) in
    fish_step tank [] n |> snd

let parse_input filename = 
    In_channel.create filename 
    |> IO.stdin_map (IO.int_line ',') 
    |> List.hd_exn 
    |> fill_tank @@ fish_tank ()

let part1 filename =  
    parse_input filename
    |> (fun tank -> fishes tank 80)
    |> List.hd_exn
    |> printf "%d\n"

let part2 filename =  
    parse_input filename
    |> (fun tank -> fishes tank 256)
    |> List.hd_exn
    |> printf "%d\n"

let () =
    let filename = "day6/input.txt" in 
    part1 filename; part2 filename
