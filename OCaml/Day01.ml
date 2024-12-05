(* using OCaml 4.13 => no Scanf, no read_lines... => abusing Arg.read_arg *)
let pair line = 
  let substrings = String.split_on_char ' ' line in
  int_of_string (List.nth substrings 0),  int_of_string (List.nth substrings 1)

let left, right = List.split (List.map pair (Array.to_list (Arg.read_arg "day01.in")))

let left  = List.sort Int.compare left
let right = List.sort Int.compare right

let distances = List.map2 (fun a b -> abs(a-b)) left right

let count x = List.fold_left (fun n y -> if y=x then n+1 else n) 0
let counts = List.map (fun x -> x * count x right) left
let sum = List.fold_left (+) 0


let () = print_int (sum distances) ; print_newline ();
         print_int (sum counts)
