let input = Arg.read_arg "day03.in" |> Array.fold_left String.cat "" 
let regexp = Str.regexp "mul(\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\),\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\))"

let rec sum_all_muls input start acc = 
  try 
    let pos = Str.search_forward regexp input start in 
    sum_all_muls input (pos+1) (acc + int_of_string (Str.matched_group 1 input) * int_of_string (Str.matched_group 2 input) )
  with Not_found -> acc

let () = sum_all_muls input 0 0 |> Printf.printf "Part 1: %d\n"

