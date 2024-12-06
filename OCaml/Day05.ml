let rule_pair string = String.split_on_char '|' string |> List.map int_of_string
let rules = Arg.read_arg "day05a.in" |> Array.to_list |> List.map rule_pair
let split_update string = String.split_on_char ',' string |> List.map int_of_string
let updates = Arg.read_arg "day05b.in" |> Array.to_list |> List.map split_update

let (<) page1 page2 = List.mem [page1;page2] rules
let middle_page update = List.nth update (List.length update / 2)
let sum = List.fold_left (+) 0
let rec is_ordered = function
  | [] | [_]        -> true
  | a::(b::_ as tl) -> a < b && is_ordered tl

let ordered_updates, unordered_updates = List.partition is_ordered updates

let () = ordered_updates |> List.map middle_page |> sum |> Printf.printf "Part 1: %d\n"

let compare page1 page2 = if page1 < page2 then -1 else if page2 < page1 then 1 else 0
let () = unordered_updates |> List.map (List.sort compare) |> List.map middle_page |> sum |> Printf.printf "Part 2: %d\n"


