                                            open Base;;
                                            open Kock;;
                                            open Stdio;;

                              let sort_string (s : string) : string =
                                          String.to_list s
                                |> List.sort ~compare:(Char.compare)
                                       |> String.of_char_list

                              let numbers = List.map ~f:sort_string [
                                             "abcefg";
                                               "cf";
                                              "acdeg";
                                              "acdfg";
                                              "bcdf";
                                              "abdfg";
                                             "abdefg";
                                               "acf";
                                             "abcdefg";
                                              "abcdfg"
                                                 ]

                       let digit_of_alpha c = Char.to_int c - Char.to_int 'a'
                   let alpha_of_digit d = Char.of_int_exn @@ Char.to_int 'a' + d

                                          let digit_map =
                              List.mapi ~f:(fun i v -> (v, i)) numbers
                                |> Map.of_alist_exn (module String)

                           let map_of_permlist (a : (char * char) list) =
                                  Map.of_alist_exn (module Char) a

                          let ( &-- ) (a : char) (b : char) : char list = 
                 List.map ~f:alpha_of_digit (0 -- (Char.to_int b - Char.to_int a))

                                       let get_perms range =
                                      Comb.permutations range
                            |> List.map ~f:(fun xs -> Lst.zip range xs)
                                   |> List.map ~f:map_of_permlist

                type permutation = (char, char, Base.Char.comparator_witness) Map.t

                                 let print_perm (p : permutation) =
                                           Map.to_alist p
                         |> Lst.print (fun (a, b) -> printf "%c, %c\n" a b)

                 let apply_permutation (p : permutation) (lst : string) : string =
                           String.map ~f:(fun x -> Map.find_exn p x) lst

                let check_perm (digits : string list) (desired) (p : permutation) =
                                          let prediction =
                       List.map ~f:(sort_string % apply_permutation p) digits
                                 |> Set.of_list (module String) in
                                  Set.equal (prediction) (desired)

                      let get_the_permutation (perms) (digits : string list) =
                        let desired = Set.of_list (module String) numbers in
                          List.filter ~f:(check_perm digits desired) perms
                                           |> List.hd_exn

                                     let read_input filename =
                                     In_channel.create filename
                                    |> IO.stdin_map (fun line ->
                                 Str.split (Str.regexp " | ") line
                                            |> (function
                  | hd::md::_ -> String.split ~on:' ' hd, String.split ~on:' ' md
                          | _ -> raise (Invalid_argument "missing part")))

                              let part1_brick perms (digits, output) =
                  let p = get_the_permutation perms (List.join [digits;output]) in
                 List.map ~f:(fun x -> sort_string @@ apply_permutation p x) output

                                        let part1 filename =
                                       let map = digit_map in
                               let perms = get_perms ('a' &-- 'h') in 
                                        read_input filename
                                      |> List.map ~f:(fun x ->
                                        part1_brick perms x
                           |> List.map ~f:(fun v -> Map.find_exn map v))
                                      |> List.map ~f:(fun x -> 
                              List.fold ~f:(fun acc n -> match n with 
                                           | 1 -> acc + 1
                                           | 4 -> acc + 1
                                           | 7 -> acc + 1
                                           | 8 -> acc + 1
                                       | _ -> acc) ~init:0 x)
                                   |> List.sum (module Int) ~f:id
                                          |> printf "%d\n"

                                        let part2 filename =
                                       let map = digit_map in
                               let perms = get_perms ('a' &-- 'h') in 
                                        read_input filename
                                      |> List.map ~f:(fun x ->
                                        part1_brick perms x
                           |> List.map ~f:(fun v -> Map.find_exn map v))
                                      |> List.map ~f:(fun x -> 
                        List.fold ~f:(fun acc n -> 10 * acc + n) ~init:0 x)
                                   |> List.sum (module Int) ~f:id
                                          |> printf "%d\n"

                                              let () =
                                 let filename = "day8/input.txt" in
                                   part1 filename; part2 filename
