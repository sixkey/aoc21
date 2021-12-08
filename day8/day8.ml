open Base;; 
open Kock;;
open Stdio;;

let add_if_empty (a : 'a list list) = match a with 
    | [] -> [[]]
    | a -> a 

let perms lst = 
    let ar = Array.of_list (List.map ~f:(fun x -> (true, x)) lst) in 
    let rec perm_step ar = 
        add_if_empty @@ List.concat @@ List.filter_map ~f:(fun i -> 
            let f, v = ar.(i) in 
            if not f 
                then None 
                else 
                    (ar.(i) <- false, v; 
                        (let res = List.map ~f:(fun xs -> v::xs) (perm_step ar) in 
                        (ar.(i) <- true, v; 
                        Some res)))
        ) (0 -- Array.length ar) in
    perm_step ar

let true_digits = 
    [
        [0, 1, 2, 4, 5, 6], 
        [2, 5], 
        [0, 2, 3, 4, 6], 
        [0, 2, 3, 5, 6], 
        [1, 2, 3, 5], 
        [0, 1, 3, 5, 6], 
        [0, 1, 3, 4, 5, 6], 
        [0, 2, 5],
        [0, 1, 2, 3, 4, 5, 6], 
        [0, 1, 2, 3, 5, 7]
    ]

let alpha_digit c = Char.to_int c - Char.to_int 'a'

let part1 filename = 
    In_channel.create filename 
    |> IO.stdin_lst 
    |> List.hd_exn 
    |> Str.split (Str.regexp " | ") 
    |> (function 
        | hd::md::_ -> String.split ~on:' ' hd, String.split ~on:' ' md
        | _ -> raise (Invalid_argument "missing part"))
    |> fun (digits, _) -> 
        List.map 
            ~f:(fun x -> List.map ~f:alpha_digit @@ String.to_list x) 
            digits
    |> Lst.print (Lst.print (fun (a) -> printf "(%d)" a)) 

let test_perms = 
    perms (0 -- 4)
    |> List.map ~f:(fun xs -> Lst.zip (0 -- 6) xs) 
    |> Lst.print (Lst.print (fun (a, b) -> printf "(%d,%d)" a b)) 

let () = 
    let filename = "day8/test.txt" in 
        part1 filename
    

