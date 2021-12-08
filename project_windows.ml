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
  
  let max l =
    List.fold_left (fun acc x -> max acc x) (-1) l
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


module Solver4 : Solver = struct
  type board = int list list

  (* generates board list from a suitable string *)
  let rec make_tables new_table (data_out : board list) = function
	| [] -> data_out
    | ""::t -> make_tables true data_out t
	| line::t -> let numbers = List.int_list ((String.split_on_char ' ' line) |> List.filter (fun s -> s <> "")) in
				 let data_out = if new_table then [numbers]::data_out else 
				   let dh::dt = data_out in 
				   (numbers::dh)::dt
				 in
				 make_tables false data_out t

  (* Checks if the table has either a complete row or a complete column on -1s *)
  let has_won table = 
	let rec transpose = function
	  | [] -> []
	  | []::xss -> transpose xss
	  | (x::xs) :: xss -> (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
	in
	
	let check_row row = 
	  List.for_all (fun x -> x = -1) row
	in
	
	let check_rows table = 
	  List.exists check_row table
	in
	check_rows table || check_rows (transpose table)
	
  let evaluate table = 
    List.fold_left (+) 0 (List.map (fun row -> List.fold_left (fun x y -> if y = -1 then x else x + y) 0 row) table)
	
  (* checks if the table has a given number, if yes, replaces it with a -1 *)
  let check_number num table =
	let check_row num row= 
	  List.map (fun x -> if x = num then -1 else x) row
	in
	List.map (check_row num) table
	
  let guess_number num tables = 
    List.map (check_number num) tables
	
  let rec guess_numbers nums tables = (* PART 1 *)
    match nums with
	| [] -> failwith "Ran out of numbers"
	| num::t -> let tables = guess_number num tables in
				let m = List.max (List.map (fun t -> if has_won t then evaluate t else -1) tables) in
				if m <> -1 then num * m else guess_numbers t tables
				
  let rec eliminate_numbers nums tables = (* PART 2 *)
    match nums with
	| [] -> failwith "Ran out of numbers"
	| num::t -> let tables = guess_number num tables in
				if (List.length tables) = 1 && has_won (List.nth tables 0) then num * evaluate (List.nth tables 0) else
				eliminate_numbers t (List.filter (fun x -> not (has_won x)) tables)
				(*
				let m = List.max (List.map (fun t -> if has_won t then evaluate t else -1) tables) in
				if m <> -1 then num * m else guess_numbers t tables
				*)
	
  
  let naloga1 data = 
    let numbers::data = List.lines data in
	let numbers = List.int_list (String.split_on_char ',' numbers) in
	let tables = make_tables true [] (List.rev data) in (* build the tables backwards *)
	string_of_int (guess_numbers numbers tables)


  let naloga2 data _part1 = 
	let numbers::data = List.lines data in
	let numbers = List.int_list (String.split_on_char ',' numbers) in
	let tables = make_tables true [] (List.rev data) in (* build the tables backwards *)
	string_of_int (eliminate_numbers numbers tables)
end


module Solver5 : Solver = struct
  type point = int*int
  type line = point*point
  
  let sign = function
    | 0 -> 0
    | n -> n / abs(n)

  
  let parse data : line list = 
	let lines = List.lines data in
	let parseline l =
	  let [a;_;b] = String.split_on_char ' ' l in
	  let [x1;y1] = String.split_on_char ',' a in
	  let [x2;y2] = String.split_on_char ',' b in
	  ((int_of_string x1, int_of_string y1), (int_of_string x2, int_of_string y2))
	in
	List.map parseline lines
  
  let rec gen_points (diag : bool) (line : line) : point list =
	match line with
	| x, y when x = y -> [x]
	| (a, b), (c, d) when b = d -> (a, b)::(gen_points diag ((a + sign (c - a), b), (c, b)))
	| (a, b), (c, d) when a = c -> (a, b)::(gen_points diag ((a, b + sign (d - b)), (c, d)))
	| (a, b), (c, d) when diag -> (a, b)::(gen_points diag ((a + sign (c - a), b + sign (d - b)), (c, d)))
	| _ -> []
	
  let visit_points visited (diag : bool) (line : line) : unit = 
    let update_counter x = 
      if Hashtbl.mem visited x then
        let current_count = Hashtbl.find visited x in
        Hashtbl.replace visited x (current_count + 1)
      else
        Hashtbl.replace visited x 1
      in
	List.iter update_counter (gen_points diag line)
    

  let naloga1 data = 
	let lines = parse data in
	let visited = Hashtbl.create 10000 in
	List.iter (visit_points visited false) lines;
	Hashtbl.filter_map_inplace (fun k v -> if v > 1 then Some v else None) visited;
	string_of_int (Hashtbl.length visited)

  let naloga2 data _part1 = 
	let lines = parse data in
	let visited = Hashtbl.create 10000 in
	List.iter (visit_points visited true) lines;
	Hashtbl.filter_map_inplace (fun k v -> if v > 1 then Some v else None) visited;
	string_of_int (Hashtbl.length visited)
end

module Solver6 : Solver = struct
  let parse data =
    List.int_list (String.split_on_char ',' data)
	
  let get_fish data = 
    let count d data = 
	  List.length (List.filter (fun x -> x = d) data)
	in
  
    List.init 9 (fun x -> count x data)
	
  let rec step data = function
	| 0 -> (List.fold_left (+) 0 (List.tl data))
    | n -> step (List.init 10 (fun x -> (List.nth data ((x + 1) mod 9)) + if x = 6 then List.nth data 0 else 0)) (n-1)

  let naloga1 data = 
	string_of_int (step (get_fish (parse data)) 80)

  let naloga2 data _part1 = 
    string_of_int (step (get_fish (parse data)) 256)
end


module Solver7 : Solver = struct
  let parse data =
    List.int_list (String.split_on_char ',' data)
	
  let rec calculate_fuel pos fin min fuel crabs = 
    if pos = fin then min else
    let dis = List.fold_left (+) 0 (List.map (fun f -> fuel (abs (f - pos))) crabs)
	in
	let min = if dis < min then dis else min in
	calculate_fuel (pos + 1) fin min fuel crabs

  let naloga1 data = 
    string_of_int (calculate_fuel 0 1000 max_int (fun x -> x) (parse data))

  let naloga2 data _part1 = 
    string_of_int (calculate_fuel 0 1000 max_int (fun x -> (x * (x+1))/2) (parse data))
end


module Solver8 : Solver = struct
  let parse data =
    List.map (fun x -> let _::t = x in t) (List.map (String.split_on_char ' ') (List.map (fun x -> List.nth x 1) (List.map (String.split_on_char '|' ) (List.lines data))))

  let naloga1 data = 
	let is_unique c x = 
	  let is_u = if String.length x = 2 || String.length x = 3 || String.length x = 4 || String.length x = 7 then 1 else 0
	  in
	  Printf.printf "Is %s unique? %d\n" x is_u;
	  c + is_u
	in
	let count_unique c lst = 
	  c + (List.fold_left is_unique 0 lst)
	in
	string_of_int (List.fold_left count_unique 0 (parse data))

  let naloga2 data _part1 = ""
end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "6" -> (module Solver6)
  | "7" -> (module Solver7)
  | "8" -> (module Solver8)
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
