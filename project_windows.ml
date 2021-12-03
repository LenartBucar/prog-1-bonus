let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l 

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver_template : Solver = struct
  let naloga1 data = ""

  let naloga2 data _part1 = ""
end

module Solver1 : Solver = struct
  let rec count_larger cnt lst1 lst2 =
    match (lst1, lst2) with
    | (_, []) -> cnt
	| (a::t1, b::t2) -> count_larger (if a < b then cnt + 1 else cnt) t1 t2
	| (_, _) -> failwith "Second list must be of at most equal length compared to the first one"

  let naloga1 data = 
	let data = List.int_list (List.lines data) in
	string_of_int (count_larger 0 data (List.tl data))

  let naloga2 data _part1 = (*The second part is the same as the first one, just that the lists are shifted over by 3 instead of by 1. As it is a rolling sum, the values present in both of them simply subtract and cancel out*)
    let data = List.int_list (List.lines data) in
	string_of_int (count_larger 0 data (List.tl (List.tl (List.tl data))))
end

module Solver2 : Solver = struct
  let rec move part (depth, hor, aim) = function
	| [] -> (depth, hor, aim)
	| (ins::dis::[])::t -> 
	  let dis = int_of_string dis in (match ins with
		| "down" -> if part = 1 then move part (depth + dis, hor, aim) t else move part (depth, hor, aim + dis)	t
		| "up" -> if part = 1 then move part (depth - dis, hor, aim) t else move part (depth, hor, aim - dis) t	
		| "forward" -> if part = 1 then move part (depth, hor + dis, aim) t else 
		  move part (depth + aim*dis, hor + dis, aim) t
		| _ -> failwith "Invalid instruction"
		)
	| _ -> failwith "List was not of the correct form"

  let naloga1 data =
	let data = List.map (String.split_on_char ' ') (List.lines data) in
	let (d, h, _) = move 1 (0, 0, 0) data in
	string_of_int (d * h)

  let naloga2 data _part1 = 
	let data = List.map (String.split_on_char ' ') (List.lines data) in
	let (d, h, _) = move 2 (0, 0, 0) data in
	string_of_int (d * h)
end

module Solver3 : Solver = struct
  let matches pos bit number = 
    number.[pos] = bit
	
  let count acc value =
    if value then acc + 1 else acc
  
  let most_common inv pos (numbers: string list) = 
    let len = List.length numbers in
	let num = List.fold_left count 0 (List.map (matches pos '1') numbers) in
	let rem = len - num in
	let out = if rem = num then '1' else if num > rem then '1' else '0' in
	if inv then (if out = '1' then '0' else '1') else out
	
  let invert number = 
    String.init (String.length number) (fun x -> if number.[x] = '1' then '0' else '1')
  
  let naloga1 data =
    let data = List.lines data in
	let len = String.length (List.nth data 0) in
	let num = String.init len (fun x -> most_common false x data) in
	num ^ " - " ^ (invert num)
	
  let rec reduce inv pos = function
    | x::[] -> x
	| l -> let common = most_common inv pos l in
		reduce inv (pos + 1) (List.filter (matches pos common) l)
	
	
  let naloga2 data _part1 = 
	let data = List.lines data in
	let num_1 = reduce false 0 data in
	let num_2 = reduce true	0 data in
	num_1 ^ " - " ^ num_2
end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()
