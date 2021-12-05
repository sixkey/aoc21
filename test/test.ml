open Kock;;
open Base;;
open Stdio;; 

let () = In_channel.create "test/dune" |> IO.stdin_map id |> List.map ~f:((flip (^)) "\n") |> IO.print_string_list;
