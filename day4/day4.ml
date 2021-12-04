                                            open Base;;
                                            open Stdio;;

                                            let id x = x

                                         let dup x = (x, x)
                                    let fdup f g x = (f x, g x)
                                          let dump _ = ()

                                      let (%) f g x = f (g x)
                                       let flip f a b = f b a

                                       let trd (_, _, c) = c
                let (--) (a : int) (b : int) = List.init (b - a) ~f:(fun x -> x + a)
                     let (---) ((ax, ay) : int * int) ((bx, by) : int * int) = 
     List.concat @@ List.init (ay - ax) ~f:(fun a -> List.init (by - bx) ~f:(fun b -> (a, b)))

                   let l_all (lst : bool list) = List.fold lst ~init:true ~f:(&&)
                   let l_or (lst : bool list) = List.fold lst ~init:false ~f:(||)
                                 let rec l_drop n l = match n with 
                                              | 0 -> l 
                                        | n -> match l with 
                                             | [] -> []
                                     | _::t -> l_drop (n - 1) t 

                let build_tran tran line_tran acc line = tran acc @@ line_tran line
                            let stdin_foldi tran start max_lines stdin = 
                                 let rec stdin_foldi_step acc m i =   
                   if m = 0 then acc else match In_channel.input_line stdin with
                |   Some line -> stdin_foldi_step (tran i acc line) (m - 1) (i + 1)
                                          |   None -> acc
                               in stdin_foldi_step start max_lines 0
                   let stdin_fold tran = stdin_foldi (fun _ acc v -> tran acc v) 
              let stdin_map tran = stdin_fold (fun acc value -> tran value :: acc) []
           let stdin_mapi tran = stdin_foldi (fun i acc value -> tran i value :: acc) []
                       let stdin_lst = stdin_fold (fun acc v -> v :: acc) []

               let print_int_list lst = List.iter ~f:(printf "%d, ") lst; printf "\n"
             let print_string_list lst = List.iter ~f:(printf "%s, ") lst; printf "\n"

                      let to_dec = List.fold ~f:(fun a v -> a * 2 + v) ~init:0 
                    let inverse = List.map ~f:(fun v -> if v = 0 then 1 else 0)

                                       let int_line s line = 
                               String.split_on_chars ~on:[s] line |> 
                          List.filter ~f:(not % (=) 0 % String.length) |> 
                                     List.map ~f:Int.of_string 
                                    let coma_ints = int_line ','

let fill_row y puzzle v = List.mapi (int_line ' ' v) ~f:(fun i v -> puzzle#m.(y).(i) <- (v, false)) 

                                         class matrix w h =
                                               object 
                   val mutable ma = Array.make_matrix ~dimx:w ~dimy:h (0, false)
                                            method w = w
                                            method h = h
                                           method m = ma
                                 method print = Array.iter ma ~f:(
                                      fun r -> Array.iter ~f:(
      fun (v, b) -> Out_channel.printf "%s %d \t" (if b then "-" else " ") v) r; printf "\n")
                                        method set_v value = 
                                        Array.iteri ma ~f:(
                                    fun y r -> Array.iteri ~f:(
                       fun x (v, b) -> ma.(y).(x) <- (v, b || v = value)) r)
                                               end;;

   let check_col matrix x = List.map ~f:(fun y -> snd matrix#m.(y).(x)) (0 -- matrix#h) |> l_all
   let check_row matrix y = List.map ~f:(fun x -> snd matrix#m.(y).(x)) (0 -- matrix#w) |> l_all
     let check_cols matrix = List.map ~f:(fun x -> check_col matrix x) (0 -- matrix#w) |> l_or
     let check_rows matrix = List.map ~f:(fun y -> check_row matrix y) (0 -- matrix#h) |> l_or
                  let check_matrix matrix = check_cols matrix || check_rows matrix

             let resolve_element m x y = let (v, b) = m#m.(y).(x) in if b then 0 else v
                        let sum_unmarked m = List.fold ~f:(fun acc (x, y) -> 
                    acc + resolve_element m x y) ~init:0 ((0, m#w) --- (0, m#h))
             let sum_puzzles = List.fold ~f:(fun acc p -> acc + sum_unmarked p) ~init:0
                             let fil_win = List.filter ~f:check_matrix 

             let puzzle_step (puzzles : matrix list) (query : int list) p_pred n_pred = 
                            let rec step puzzles query p_pred n_pred i = 
                       let winning_puzzles = List.filter ~f:p_pred puzzles in
                            if n_pred (List.length winning_puzzles) then 
                                         winning_puzzles, i 
                                                else
                                          match query with 
  |   hd::tl -> List.iter ~f:(fun p -> p#set_v hd) puzzles; step puzzles tl p_pred n_pred (i + 1)
                                           |   _ -> [], i
                               in step puzzles query p_pred n_pred 0

                                     let parse_puzzle s stdin = 
                                   let puzzle = new matrix s s in 
                 snd (stdin_mapi (fun i v -> fill_row i puzzle v) s stdin, puzzle)

                           let rec parse_puzzles s n stdin = match n with 
                                            |   0 -> []
                   |   n -> parse_puzzle s stdin :: parse_puzzles s (n - 1) stdin

                                     let parse_file s n stdin = 
                     let query = List.hd_exn @@ stdin_map coma_ints 1 stdin in
                              let puzzles = parse_puzzles s n stdin in
                                           query, puzzles

                             let solve p_pred n_pred (query, puzzles) = 
                  let winning, index = puzzle_step puzzles query p_pred n_pred in
                                      (query, winning, index)

                                        let part1 name s n = 
                                       In_channel.create name 
                                         |> parse_file s n  
                           |> solve check_matrix (fun x -> Poly.(=) x 1) 
                             |> fun (query, winning_puzzles, index) -> 
         (sum_puzzles winning_puzzles) * (List.nth_exn query (index - 1))  |> printf "%d\n"

                                        let part2 name s n = 
                                       In_channel.create name 
                                         |> parse_file s n
                       |> solve (not % check_matrix) (fun x -> Poly.(=) x 1) 
                             |> fun (query, winning_puzzles, index) -> 
         solve (check_matrix) (fun x -> Poly.(=) x 1) (l_drop index query, winning_puzzles) 
                                  |> fun (n_query, _, n_index) -> 
       (sum_puzzles winning_puzzles) * (List.nth_exn n_query (n_index - 1))  |> printf "%d\n"

                                              let () = 
                                 let filename = "day4/input.txt" in
                                            let s = 5 in 
                                           let n = 100 in
                               part1 filename s n; part2 filename s n
