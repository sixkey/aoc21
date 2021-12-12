open Base;;
open Kock;;
open Stdio;;
open Poly;;

module StringGraph = Graph.Make(struct     
    type t = string [@@deriving compare, sexp_of]
end)

let add_line g l = 
    String.split ~on:'-' l
    |> function 
        | f::s::_ -> StringGraph.add_edge g (f, s) 
        | _ -> raise (Invalid_argument "the line has to contain '-'")

let rec dfs (g : StringGraph.t) (visited : StringGraph.vst) (vertex : StringGraph.vt) = 
    if vertex = "end" then 
        1
    else if Set.mem visited vertex then 
        0
    else 
        let new_visited = if KStr.is_lower vertex 
            then Set.add visited vertex 
            else visited in
        List.map ~f:(dfs g new_visited) (Map.find_exn g.es vertex)
        |> List.sum (module Int) ~f:id

let draw_graph (g : StringGraph.t) = 
    Map.iter_keys g.es ~f:(fun a -> List.iter (Map.find_exn g.es a) ~f:(
        fun b -> printf "%s -> %s\n" a b
    ))

let part1 filename = 
    In_channel.create filename 
    |> IO.stdin_fold add_line StringGraph.empty
    |> fun g -> dfs g (Set.empty (module StringGraph.Comparator)) "start"
    |> printf "%d\n"

let rec dfs2 
        (g : StringGraph.t) 
        (visited : StringGraph.vst) 
        (depth : int)
        (twice_used : bool)
        (vertex : StringGraph.vt) = 
    if vertex = "end" then 
        1
    else if (vertex = "start" && depth <> 0) || (Set.mem visited vertex && twice_used) then
        0
    else 
        let twice_used_new = Set.mem visited vertex || twice_used in 
        let new_visited = if KStr.is_lower vertex
                then Set.add visited vertex
            else visited in
        List.map ~f:(dfs2 g new_visited (depth + 1) twice_used_new) (Map.find_exn g.es vertex) 
        |> List.sum (module Int) ~f:id

let part2 filename = 
    In_channel.create filename 
    |> IO.stdin_fold add_line StringGraph.empty
    |> fun g -> dfs2 g (Set.empty (module StringGraph.Comparator)) 0 false "start" 
    |> printf "%d\n"

let () = 
    let filename = "day12/input.txt" in
    part1 filename; part2 filename
