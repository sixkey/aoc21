let (%) f g = function x -> f (g x)

let rec factorial n = 
    if n <= 1 then 1 else n * factorial (n - 1);;

let str_factorial = string_of_int % factorial;;

let () = 5 |> (string_of_int % factorial) |> print_endline;;
