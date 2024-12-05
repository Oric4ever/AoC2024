let parse_line line = String.split_on_char ' ' line |> List.map int_of_string

let input = Arg.read_arg "day02.in" |> Array.to_list |> List.map parse_line

let rec is_increasing = function 
  | [] | [_] -> true
  | x :: (y :: _ as tl) when y > x && y <= x+3 -> is_increasing tl
  | _ -> false

let rec is_decreasing = function
  | [] | [_] -> true
  | x :: (y :: _ as tl) when y < x && y >= x-3 -> is_decreasing tl
  | _ -> false

let is_safe report = is_increasing report || is_decreasing report

let () = List.filter is_safe input |> List.length |> Printf.printf "Part 1: %d \n"

let rec remove_level lst index e = if index = 0 then List.tl lst else List.hd lst :: remove_level (List.tl lst) (index-1) e

let or_map = List.fold_left (||) false
let is_nearly_safe report = List.mapi (remove_level report) report |> List.map is_safe |> or_map

let () = List.filter is_nearly_safe input |> List.length |> Printf.printf "Part 2: %d \n"