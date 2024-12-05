let input = Arg.read_arg "day03.in" |> Array.fold_left String.cat "" 
let mul_rx = Str.regexp "mul(\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\),\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\))"
let do_rx = Str.regexp "do()"
let dont_rx = Str.regexp "don't()"

let rec sum_all_muls start acc input = 
  try 
    let pos = Str.search_forward mul_rx input start in
    let mul = int_of_string (Str.matched_group 1 input) * int_of_string (Str.matched_group 2 input) in
    sum_all_muls (pos+1) (acc + mul) input
  with Not_found -> acc

let rec remove_dont_parts input start acc = 
  try 
    let pos = Str.search_forward do_rx input start in 
    keep_do_parts input pos acc
  with Not_found -> acc
and keep_do_parts input start acc = 
  try
    let pos = Str.search_forward dont_rx input start in
    remove_dont_parts input pos (acc ^ String.sub input start (pos-start))
  with Not_found -> acc ^ String.sub input start (String.length input - start)
  
let () = sum_all_muls 0 0 input |> Printf.printf "Part 1: %d\n" ;
         keep_do_parts input 0 "" |> sum_all_muls 0 0 |> Printf.printf "Part 2: %d\n"

