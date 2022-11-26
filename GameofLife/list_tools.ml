(*1.1 Basics*)

let length l = 
  match l with
    [] -> 0
  |_-> let rec aux l n = 
         match l with 
           [] -> n
         |e::l -> aux l (n+1)
      in aux l 0;;

let rec nth p l =
  if p < 0 then invalid_arg "nth: index must be a natural"
  else
    match l with
      []-> failwith "nth: list is too short"
    |e::l -> if p = 0 then e 
        else nth (p-1) l ;; 

let is_pos list =
match list with
[] -> invalid_arg""
|_ -> let rec aux list =
match list with
[] -> true
|e::l -> if e < 0 then false
else aux l 
in aux list;;

let rec get_max list = 
match list with
[] -> invalid_arg ""
|[n] -> n
|a::b::l -> if a >= b then get_max (a::l)
else
get_max (b::l);;

(*1.2 Build - Modify*)

let init_list n e =
  if n < 0 then invalid_arg "init_list : n must be a natural"
  else
    let rec aux n1 l =
      match n1 with
        _ when n1 = n -> l
      |_ -> let l = e::l in aux (n1+1) l
    in aux 0 [];;

let rec put_list v i l =
  match l with
    [] when i = 0 -> [v]
  |[] -> []
  |e::l -> if i = 0 then v :: put_list v (i-1) l 
      else e :: put_list v (i-1) l ;;

let rec append l1 l2 =
match l1 with
[] -> l2
|e::l -> e::append l l2;;

(*1.3 'a list list*)

let rec init_board (l, c) x =
  if l < 0 || c < 0 then invalid_arg "init_board: l and c must be whole numbers"
  else
    match c with
      0 -> [[]]
    |1 -> [init_list l x]
    |_-> init_list l x::init_board (l, c-1) x;;

let rec is_board board =
  match board with
    [] -> true
  |[_]-> true
  |a::b::l -> if length (a) = length(b) then is_board (a::l)
      else false;;

let rec print_board l =
  match l with
    []-> print_string ""
  |e::l -> let rec aux x =
             match x with
               [] -> print_newline();print_board(l)
             |p::x -> print_char(p); aux(x)
      in aux e;;

let get_cell (a, b) board =
  if a < 0 || b < 0 then invalid_arg "get_cell: a and b must be whole numbers"
  else
    let aux = nth (a) board in nth (b) aux;;

let put_cell v (a, b) board =
  if a < 0 || b < 0 then invalid_arg "put_cell: a and b must be whole numbers"
  else
    match board with
      board when a >= length board -> board
    |e::l when b >= length e -> board
    |board -> put_list ( put_list v (b) (nth a board)) a board;;