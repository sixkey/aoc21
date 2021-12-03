                                       let read_line stdin = 
                                  try Some (input_line stdin) with 
                                       End_of_file -> None;;

                                   let process_stdin tran stdin = 
                                       let rec input lines =
                                     match read_line stdin with 
                             Some line -> input ((tran line) :: lines)
                                     |   None -> List.rev lines 
                                                 in 
                                             input [];;

                                       let print_int_list l = 
                                      List.map string_of_int l 
                                       |> String.concat "\n" 
                                          |> print_string

                                      let maybe_greater a b = 
                                            match b with
                                          | None -> false 
                                         | Some n -> a > n 


                                let rec count_growing last numbers =
                                         match numbers with
                                             | [] -> 0 
           | h :: t -> (if maybe_greater h last then 1 else 0) + count_growing (Some h) t

                                      let rec merge numbers = 
                                         match numbers with 
                     | a :: b :: c :: t -> (a + b + c) :: (merge (b :: c :: t))
                                             | _ -> [] 

                                              let () = 
                                       (open_in "input.txt") 
                                   |> process_stdin int_of_string 
                                              |> merge 
                                       |> count_growing None 
                                          |> string_of_int 
                                          |> print_endline
