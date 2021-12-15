open Base;;
open Stdio;; 
open Kock;;
open Poly;;

let alphabet_size = 26

let letter_of_idx idx = Char.of_int (idx + Char.to_int 'A')
let idx_of_letter letter = Char.to_int letter - Char.to_int 'A'


let idx_of_pair a b = a * alphabet_size + b
let pair_of_idx idx = (idx / alphabet_size, Base.(%) idx alphabet_size)

let parse_rule line = 
    Str.split (Str.regexp " -> ") line
    |> function 
        [f;t] -> 
            (match List.map ~f:idx_of_letter @@ List.concat [ String.to_list f ; String.to_list t ] with 
            [a; b; c] -> ((idx_of_pair a b, idx_of_pair a c),
                          (idx_of_pair a b, idx_of_pair c b))
            | _ -> raise @@ Invalid_argument "incorrect line")
        | _ -> raise @@ Invalid_argument "incorrect line"

let add_points (mat : int Matrix.t) ((ax, ay), (_, by)) =
    Matrix.apply_col (fun i _ -> if i = ay || i = by then 1 else 0) ax mat

let word_to_vec line = 
    String.to_list line
    |> List.map ~f:idx_of_letter
    |> Lst.pairs_map idx_of_pair
    |> List.fold ~f:(fun mat i -> mat.m.(0).(i) <- mat.m.(0).(i) + 1; mat) ~init:(Matrix.make_col (alphabet_size * alphabet_size) 0)

let start_end str = 
    String.of_char_list [ String.get str 0 ; String.get str ((String.length str) - 1) ] 

let read_input filename = 
    In_channel.create filename
    |> (share_pass @@ IO.stdin_foldi_while (fun _ acc line -> if line = "" then None else Some (String.strip line :: acc)) [])
    |> second @@ fst
    |> second @@ List.hd_exn
    |> second @@ fdup id word_to_vec 
    |> first @@ IO.stdin_fold (fun acc line -> parse_rule line |> add_points acc) (Matrix.ident (alphabet_size * alphabet_size))

let count_letters (word_mat : int Matrix.t) = 
    let res = Matrix.make_col alphabet_size 0 in 
    for i = 0 to alphabet_size - 1 do 
        for j = 0 to alphabet_size - 1 do 
            res.m.(0).(i) <- res.m.(0).(i) + word_mat.m.(0).(i * alphabet_size + j);
        done
    done;
    res

let hist_map word = 
    let res = Matrix.make_col alphabet_size 0 in 
    String.iter ~f:(fun letter -> 
        idx_of_letter letter
        |> fun i -> 
            res.m.(0).(i) <- res.m.(0).(i) + 1) word;
    res

let rec cycle t v = function 
    0 -> v 
    | n -> cycle t (Matrix.mul t v) (n - 1)

let str_last str = 
    String.get str ((String.length str) - 1)

let solve filename cycles = 
    let transform, (word, word_mat) = read_input filename in 
    let word_add = hist_map @@ String.of_char @@ str_last word in 
    cycle transform word_mat cycles
    |> count_letters
    |> Matrix.add word_add
    |> Matrix.to_listi
    |> List.map ~f:trd
    |> List.filter ~f:(fun x -> x <> 0)
    |> fdup (List.max_elt ~compare:Int.compare) (List.min_elt ~compare:Int.compare)
    |> (function 
        Some max, Some min -> max - min
        | _ -> -1)
    |> printf "%d\n"

let () = 
    let filename = "day14/input.txt" in 
    solve filename 10; solve filename 40
