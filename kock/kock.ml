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

(*Tuple manipulation*)

let trd (_, _, c) = c

(*Ranges*)

let (--) (a : int) (b : int) = List.init (b - a) ~f:(fun x -> x + a)
let (---) ((ax, ay) : int * int) ((bx, by) : int * int) = 
    List.concat @@ List.init (ay - ax) ~f:(fun a -> List.init (by - bx) ~f:(fun b -> (ax + a, bx + b)))

(* Lists *)

module Lst = struct 

    let l_all (lst : bool list) = List.fold lst ~init:true ~f:(&&)
    let l_or (lst : bool list) = List.fold lst ~init:false ~f:(||)

    let rec l_drop n l = match n with 
    | 0 -> l 
    | n -> match l with 
    | [] -> []
    | _::t -> l_drop (n - 1) t 

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

let to_dec (xs : int list) : int = List.fold ~f:(fun a v -> a * 2 + v) ~init:0 xs
let inverse (xs : int list) : int list = List.map ~f:(fun v -> if v = 0 then 1 else 0) xs
