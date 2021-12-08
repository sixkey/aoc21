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

let numbers = [
    "abcefg"; 
    "cf"; 
    "acdeg"; 
    "acbfg"; 
    "bcdf"; 
    "abdfg"; 
    "abdefg"; 
    "acf"; 
    "abcdefg"; 
    "abcdfg"
]

let digit_of_alpha c = Char.to_int c - Char.to_int 'a'
let alpha_of_digit d = Char.of_int_exn @@ Char.to_int 'a' + d

let dl_of_ds s = 
    String.to_list s
    |> List.map ~f:digit_of_alpha 
    |> List.sort ~compare:(Int.compare)

let ds_of_dl d = 
    List.map ~f:alpha_of_digit d
    |> List.sort ~compare:(Char.compare)
    |> String.of_char_list

let true_digits = List.map ~f:dl_of_ds numbers

let digit_map = 
    List.mapi ~f:(fun i v -> (v, i)) numbers
    |> Map.of_alist_exn (module String)

let sort_string (s : string) : string = 
    String.to_list s
    |> List.sort ~compare:(Char.compare)
    |> String.of_char_list

let map_of_permlist (a : (int * int) list) = 
    Map.of_alist_exn (module Int) a 

let get_perms n = 
    perms (0 -- n)
    |> List.map ~f:(fun xs -> Lst.zip (0 -- n) xs) 
    |> List.map ~f:map_of_permlist 

type permutation = (int, int, Base.Int.comparator_witness) Map.t

let print_perm (p : permutation) = 
    Map.to_alist p
    |> Lst.print (fun (a, b) -> printf "%d, %d\n" a b)


let apply_permutation (p : permutation) (lst : int list) : int list = 
    List.map ~f:(fun x -> Map.find_exn p x) lst

let check_perm (digits : string list) (desired) (p : permutation) = 
    let prediction = 
        List.map ~f:(fun ds -> 
            dl_of_ds ds
            |> apply_permutation p
            |> ds_of_dl) digits
        |> Set.of_list (module String) in
    Set.equal (prediction) (desired)

let get_the_permutation (digits : string list) = 
    let desired = Set.of_list (module String) numbers in
    let perms = get_perms 7 in 
    List.filter ~f:(check_perm digits desired) perms
    |> List.hd_exn

let read_input filename = 
    In_channel.create filename 
    |> IO.stdin_map (fun line -> 
        Str.split (Str.regexp " | ") line 
        |> (function 
            | hd::md::_ -> String.split ~on:' ' hd, String.split ~on:' ' md
            | _ -> raise (Invalid_argument "missing part")))

let part1_brick (digits, output) = 
    let p = get_the_permutation digits in 
    print_perm p; 
    List.map ~f:(fun x -> 
        printf "%s\n" x;
        dl_of_ds x
        |> apply_permutation p
        |> ds_of_dl
        |> fun x -> printf "%s\n" x; x) output
    

let part1 filename = 
    let map = digit_map in 
    read_input filename
    |> List.concat_map ~f:(fun x -> 
        part1_brick x
        |> List.map ~f:(fun v -> Map.find_exn map v))
    |> IO.print_int_list 
    

let () = 
    let filename = "day8/test.txt" in 
        part1 filename
