open Base;;
open Stdio;;

(* Misc *)

let id x = x
let dump _ = ()

(* Duplication *)

let dup x = (x, x)
let fdup f g x = (f x, g x)

(*Function manipulation*)

let (%) f g x = f (g x)
let flip f a b = f b a

let uncurry f a b = f (a, b)
let curry f (a, b) = f a b 

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

(*Tuple manipulation*)

let trd (_, _, c) = c

(*Ranges*)

let (--) (a : int) (b : int) = List.init (b - a) ~f:(fun x -> x + a)
let (---) ((ax, ay) : int * int) ((bx, by) : int * int) = 
    List.concat @@ List.init (ay - ax) ~f:(fun a -> List.init (by - bx) ~f:(fun b -> (ax + a, bx + b)))

(* Lists *)

module Lst = struct 

    let l_all (lst : bool list) = List.fold lst ~init:true ~f:(&&)
    let all_map (pred : 'a -> bool) (lst : 'a list) = List.fold ~f:(fun acc v -> acc && pred v) ~init:true lst
    let l_or (lst : bool list) = List.fold lst ~init:false ~f:(||)
    let or_map (pred : 'a -> bool) (lst : 'a list) = List.fold ~f:(fun acc v -> acc || pred v) ~init:false lst

    let rec drop n l = match n with 
    | 0 -> l 
    | n -> match l with 
    | [] -> []
    | _::t -> drop (n - 1) t 

    let max_exn compare lst = match List.max_elt ~compare:compare lst with 
        | Some v -> v 
        | None -> raise (Invalid_argument "lst has to have values")

    let min_exn compare lst = match List.min_elt ~compare:compare lst with 
        | Some v -> v 
        | None -> raise (Invalid_argument "lst has to have values")

    let print p l = printf "["; List.iter ~f:(fun x -> p x; printf ",") l; printf "]\n"

    let rec zip xs ys = match xs, ys with 
        | (xh::xt, yh::yt) -> (xh, yh) :: zip xt yt
        | (_, _) -> []

    let min_exn comp xs = match List.min_elt ~compare:comp xs with 
        | Some v -> v 
        | None -> raise (Invalid_argument "the list is empty")

end 

module Matrix = struct
    type 'a t = {m : 'a array array; w : int; h : int } 

    let make w h v = let m = Array.make_matrix ~dimx:w ~dimy:h v in { m = m ; w = w ; h = h }

    let print {m; w; h} = 
        for y = 0 to h - 1 do 
            for x = 0 to w - 1 do 
                printf (if x <> 0 then ", %d" else "%d") m.(x).(y)
            done;
            printf "\n" 
        done


    let get {m; w; h} x y = if x < 0 || x >= w || y < 0 || y >= h then None else Some m.(x).(y) 
    let get_def m x y d = match get m x y with 
        | None -> d 
        | Some v -> v 

    let to_listi {m; w; h} = List.fold ~f:(fun acc (x, y) -> (x, y, m.(x).(y)) :: acc) ~init:[] ((0, w) --- (0, h))
end

(* IO *)

module IO = struct 

    let build_tran tran line_tran acc line = tran acc @@ line_tran line

    let stdin_foldim max_lines tran start stdin = 
        let rec stdin_foldi_step acc m i =   
            if m = 0 then acc else match In_channel.input_line stdin with
                |   Some line -> stdin_foldi_step (tran i acc line) (m - 1) (i + 1)
                |   None -> acc
            in stdin_foldi_step start max_lines 0
    let stdin_foldi tran = stdin_foldim (-1) tran 
    let stdin_foldm max_lines tran = stdin_foldim max_lines (fun _ acc v -> tran acc v) 
    let stdin_fold tran = stdin_foldm (-1) tran 

    let stdin_mapim max_lines tran = stdin_foldim max_lines (fun i acc value -> tran i value :: acc) []
    let stdin_mapi tran = stdin_mapim (-1) tran
    let stdin_mapm max_lines tran = stdin_mapim max_lines (fun _ value -> tran value) 
    let stdin_map (tran : 'a -> 'b) = stdin_mapm (-1) tran

    let stdin_filter_map tran = stdin_fold (fun acc value -> 
        match tran value with 
            | Some res -> res :: acc
            | None -> acc 
    ) []

    let stdin_lst = stdin_map id

    let print_int_list lst = List.iter ~f:(printf "%d, ") lst; printf "\n"
    let print_int_array arr = Array.iter ~f:(printf "%d, ") arr; printf "\n"

    let print_string_list lst = List.iter ~f:(printf "%s, ") lst; printf "\n"

    let int_line s line = String.split_on_chars ~on:[s] line |> 
        List.filter ~f:(not % (=) 0 % String.length) |> 
        List.map ~f:Int.of_string 

end

module Comb = struct 
    
    let permutations lst = 
        let ar = Array.of_list 
            (List.map ~f:(fun x -> (true, x)) lst) in 
        let add_if_empty = function [] -> [[]] | a -> a in
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
end

let to_dec (xs : int list) : int = List.fold ~f:(fun a v -> a * 2 + v) ~init:0 xs
let inverse (xs : int list) : int list = List.map ~f:(fun v -> if v = 0 then 1 else 0) xs
