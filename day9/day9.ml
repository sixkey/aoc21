                                            open Base;;
                                            open Kock;;
                                            open Stdio;;

                                        let intv_of_char v = 
                                           Char.to_int v 
                                 |> (Fn.flip (-)) (Char.to_int '0')

             let neighbours_of x y = [ x - 1 , y ; x , y - 1 ; x + 1 , y ; x , y + 1 ]

                                     let read_input filename = 
                                     In_channel.create filename 
                               |> IO.stdin_fold (fun (h, _, vs) v -> 
    (h + 1, String.length v, ((List.map ~f:intv_of_char % String.to_list) v) :: vs)) (0, 0, [])
                                     |> (fun (h, w, values) -> 
                                 let matrix = Matrix.make w h 0 in 
     List.iteri ~f:(fun y row -> List.iteri ~f:(fun x v -> matrix.m.(x).(y) <- v) row) values; 
                                              matrix)

                                let low_points (m : int Matrix.t) = 
                                     let low_points = ref [] in 
                                      for y = 0 to m.h - 1 do 
                                      for x = 0 to m.w - 1 do
        let ns = List.map ~f:(fun (x, y) -> Matrix.get_def m x y 10) @@ neighbours_of x y in 
                                       let v = m.m.(x).(y) in 
                              if Lst.all_map (fun n -> n > v) ns then
                                low_points := (x, y) :: !low_points 
                                                done
                                               done;
                                            !low_points

                                        let part1 filename = 
                                        read_input filename 
                                       |> fdup id low_points 
                |> (fun ((matrix, low_points) : int Matrix.t * (int * int) list) -> 
          List.fold ~f:(fun acc (x, y) -> acc + matrix.m.(x).(y) + 1) ~init:0 low_points)
                                          |> printf "%d\n" 

         let rec explore_low_point (matrix : int Matrix.t) (colors : int Matrix.t) (x, y) = 
                                  match Matrix.get matrix x y with 
                                            | None -> 0
                                           | Some 9 -> 0
                                            | Some v ->  
                                    match colors.m.(x).(y) with 
                                               | 0 -> 
                                       colors.m.(x).(y) <- 1;
                                                1 + 
                                         (neighbours_of x y 
            |> List.filter ~f:(fun (nx, ny) -> (Matrix.get_def matrix nx ny (-1) >= v))
                          |> List.map ~f:(explore_low_point matrix colors)
                                  |> List.sum (module Int) ~f:id)
                                              | _ -> 0 

              let get_basins (matrix : int Matrix.t) (low_points : (int * int) list) = 
                        let colors = Matrix.make (matrix.w) (matrix.h) 0 in
                      List.map ~f:(explore_low_point matrix colors) low_points

                                        let part2 filename =
                                        read_input filename 
                                       |> fdup id low_points 
                                      |> fun (mat, points) -> 
                                       get_basins mat points
                            |> List.sort ~compare:(Fn.flip Int.compare) 
                                      |> (Fn.flip List.take) 3
                              |> List.reduce_exn ~f:(fun a b -> a * b)
                                          |> printf "%d\n"

                                              let () = 
                                 let filename = "day9/input.txt" in 
                                   part1 filename; part2 filename

