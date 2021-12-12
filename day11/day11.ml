                                            open Base;;
                                            open Stdio;;
                                            open Kock;;

                                     let read_input filename = 
                                     In_channel.create filename 
                               |> IO.stdin_fold (fun (h, _, vs) v -> 
  (h + 1, String.length v, ((List.map ~f:Conv.intv_of_char % String.to_list) v) :: vs)) (0, 0, [])
                                     |> (fun (h, w, values) -> 
                                 let matrix = Matrix.make w h 0 in 
     List.iteri ~f:(fun y row -> List.iteri ~f:(fun x v -> matrix.m.(x).(y) <- v) row) values; 
                                              matrix)

                   let colors_for (mat : 'a Matrix.t) = Matrix.make mat.w mat.h 0

       let rec explode (matrix : int Matrix.t) (colors : int Matrix.t) (x : int) (y : int) =
                                       colors.m.(x).(y) <- 1;

                                   List.iter ~f:(fun (nx, ny) -> 
                            matrix.m.(nx).(ny) <- matrix.m.(nx).(ny) + 1
                              ) @@ Matrix.get_8neigbbours matrix x y; 

                                   List.iter ~f:(fun (nx, ny) -> 
                      if colors.m.(nx).(ny) = 0 && matrix.m.(nx).(ny) > 9 then 
                                    explode matrix colors nx ny
                               ) @@ Matrix.get_8neigbbours matrix x y

                             let solve_nines (matrix : int Matrix.t) = 
                                 let colors = colors_for matrix in
                                    for y = 0 to matrix.h - 1 do 
                                    for x = 0 to matrix.w - 1 do 
                          if colors.m.(x).(y) = 0 && matrix.m.(x).(y) > 9 
                                   then explode matrix colors x y
                                              else ()
                                                done
                                               done;
                                               matrix

                                           let step mat = 
                                 Matrix.apply (fun x -> x + 1) mat
                                           |> solve_nines
           |> Matrix.apply_fold (fun acc x -> if x > 9 then (acc + 1, 0) else (acc, x)) 0

                                 let rec steps n mat = match n with 
                                          | 0 -> (mat, 0)
   | n -> step mat |> fun (matrix, count) -> second (fun x -> x + count) @@ steps (n - 1) matrix 

                                     let rec find_sync n mat = 
                                  let (nmat, count) = step mat in 
                                  if count = (mat.w * mat.h) then 
                                                 n 
                                    else find_sync (n + 1) nmat

                                        let part1 filename =
                                        read_input filename 
                                            |> steps 100 
                                        |> fun (_, count) -> 
                                        printf "%d\n" count

                                        let part2 filename = 
                                        read_input filename 
                                           |> find_sync 1 
                                          |> printf "%d\n"

                                              let () = 
                                let filename = "day11/input.txt" in
                                   part1 filename; part2 filename
