                                                      open Base;;
                                                      open Stdio;;

                                                     let id x = x;;

                                              let fdup f g x = (f x, g x)

                                                   let dup x = (x, x)

                                                let (%) f g x = f (g x)

                          let build_tran tran line_tran acc line = tran acc @@ line_tran line

                         let rec stdin_map tran stdin = match In_channel.input_line stdin with 
                                   |   Some line -> tran line :: stdin_map tran stdin
                                                     |   None -> []

                      let rec stdin_fold tran start stdin = match In_channel.input_line stdin with
                                |   Some line -> stdin_fold tran (tran start line) stdin
                                                   |   None -> start

                                let to_dec = List.fold ~f:(fun a v -> a * 2 + v) ~init:0 
                              let inverse = List.map ~f:(fun v -> if v = 0 then 1 else 0)

     let print_int_list l = List.iter ~f:(fun x -> Caml.print_int x; Caml.print_char ' ') l; Caml.print_char '\n';;

         let parse_line l = String.to_list l |> List.map ~f:((fun x -> if x = 49 then 1 else 0) % Char.to_int) 

                        let count = List.map2_exn ~f:(fun a b -> a + (if b = 1 then 1 else -1))
                               let to_zo = List.map ~f:(fun v -> if v >= 0 then 1 else 0)

                                             let rec zeros n = match n with
                                                      |   0 -> []
                                              |   a -> 0 :: zeros (a - 1)

                                                  let head = function
                                                     |   h::_ -> h
                                                      |   [] -> -1

                                                  let tail = function 
                                                     |   _::t -> t
                                                      |   [] -> []

                                        let head_d p v = if v = p then 1 else -1
                   let count_head p = List.fold ~f:(fun a (v, _) -> a + (head v |> head_d p)) ~init:0
                             let common_head p l = if count_head 1 l >= 0 then p else 1 - p

                                        let rec filter_head xs v = match xs with 
                  |   (a, b)::t -> if head a = v then (a, b) :: (filter_head t v) else filter_head t v
                                                      |   vs -> vs

                                 let rec find p m l = if m = 0 then l else match l with 
                                             |   _::_::_ -> common_head p l 
                                                    |> filter_head l 
                                        |> List.map ~f:(fun (a, b) -> tail a, b) 
                                                   |> find p (m - 1)
                                                       |   a -> a

                                 let day3_a filename width = In_channel.create filename 
                               |> stdin_fold (build_tran count parse_line) (zeros width) 
                                                        |> to_zo 
                                           |> fdup to_dec (to_dec % inverse)
                                                |> (fun (a, b) -> a * b) 
                                                  |> Caml.print_int;;

                                    let day3_b filename = In_channel.create filename 
                                                |> stdin_map parse_line 
                                          |> fdup (fun l -> List.map ~f:dup l
                                                      |> find 1 15
                                             |> List.map ~f:(to_dec % snd))
                                              (fun l -> List.map ~f:dup l
                                                      |> find 0 15
                                             |> List.map ~f:(to_dec % snd))
                                |> fun (a, b) -> List.map2_exn ~f:(fun u v -> u * v) a b
                                           |> List.iter ~f:(Caml.print_int);;

                                             let () = day3_b "input.txt";;
