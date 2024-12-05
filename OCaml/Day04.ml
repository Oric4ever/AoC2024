let string_to_array s = String.to_seq s |> Array.of_seq
let grid = Arg.read_arg "day04.in" |> Array.map string_to_array
let size = 140

let xmas_right x y = x<size-3 && grid.(y).(x+1)='M' && grid.(y).(x+2)='A' && grid.(y).(x+3)='S'
let xmas_left  x y = x >= 3   && grid.(y).(x-1)='M' && grid.(y).(x-2)='A' && grid.(y).(x-3)='S'
let xmas_down  x y = y<size-3 && grid.(y+1).(x)='M' && grid.(y+2).(x)='A' && grid.(y+3).(x)='S'
let xmas_up    x y = y >= 3   && grid.(y-1).(x)='M' && grid.(y-2).(x)='A' && grid.(y-3).(x)='S'
let xmas_dr    x y = x<size-3 && y<size-3 && grid.(y+1).(x+1)='M' && grid.(y+2).(x+2)='A' && grid.(y+3).(x+3)='S'
let xmas_dl    x y = x >= 3   && y<size-3 && grid.(y+1).(x-1)='M' && grid.(y+2).(x-2)='A' && grid.(y+3).(x-3)='S'
let xmas_ur    x y = x<size-3 && y >= 3   && grid.(y-1).(x+1)='M' && grid.(y-2).(x+2)='A' && grid.(y-3).(x+3)='S'
let xmas_ul    x y = x >= 3   && y >= 3   && grid.(y-1).(x-1)='M' && grid.(y-2).(x-2)='A' && grid.(y-3).(x-3)='S'

let dirs = [xmas_right; xmas_left; xmas_down; xmas_up; xmas_dr; xmas_dl; xmas_ur; xmas_ul]
let count x y = if grid.(y).(x)='X' then List.filter (fun f -> f x y) dirs |> List.length else 0

let sum = Array.fold_left (+) 0
let count_all = Array.mapi (fun y line -> Array.mapi (fun x _ -> count x y) line) grid
let () = Array.map sum count_all |> sum |> Printf.printf "Part 1: %d\n"