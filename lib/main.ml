(* defining operation as the 4 common operators *)
type op =
  | Plus
  | Minus
  | Mult
  | Div

(* exp as a constant or as a binary operation *)
type exp =
  | Const of int
  | Binary of exp * op * exp

type equation = exp * exp

let apply (op : op) (x : float) (y : float) : float option = match op with
  | Plus -> Some (x +. y)
  | Minus -> Some (x -. y)
  | Mult -> Some (x *. y)
  | Div when y = 0.0 -> None
  | Div -> Some (x /. y)

(* add parentheses if needed *)
let addParen (b : bool) (s : string) : string = if b then "(" ^ s ^ ")" else s

(* sets lower precedence for add, sub*)
let precedence (op : op) : int = match op with
  | Plus | Minus -> 1
  | Mult | Div -> 2

(* op to string version *)
let opToStr (op : op) : string = match op with
  | Plus -> " + "
  | Minus -> " - "
  | Mult -> " * "
  | Div -> " / "

(* converts exp into string *)
let rec expToStr (p : int) (e : exp) : string = match e with
  | Const n -> string_of_int n
  | Binary (e1, op, e2) ->
    let s1 = expToStr (precedence op) e1 in
    let s2 = expToStr ((precedence op) + 1) e2 in
    let s = s1 ^ opToStr op ^ s2 in
    addParen (p > precedence op) s

let showEq ((l, r) : (exp * exp)) : string =
  let s1 = expToStr 0 l in
  let s2 = expToStr 0 r in
  s1 ^ " = " ^ s2

let rec take (n : int) (l : int list) : int list =
  if n <= 0 then []
  else match l with
    | [] -> []
    | x::xs -> x :: take (n-1) xs

let rec drop (n : int) (l : int list) : int list =
  if n <= 0 then l
  else match l with
    | [] -> []
    | _::xs -> drop (n-1) xs

let splits (l : int list) : (int list * int list) list =
  let len = List.length l in
  let rec aux i acc =
    if i >= len then acc
    else
      let l1 = take i l in
      let l2 = drop i l in
      aux (i+1) ((l1, l2)::acc)
  in aux 1 []

let float_equal a b =
  abs_float (a -. b) < 1e-9

let rec exprs (l : int list) : (exp * float) list = match l with
  | [n] -> [ (Const n, float_of_int n) ]
  | _ ->
    let split_list = splits l in
    List.concat_map (fun (l1, l2) ->
      let exprs1 = exprs l1 in
      let exprs2 = exprs l2 in
      List.concat_map (fun (e1, v1) ->
        List.concat_map (fun (e2, v2) ->
          List.concat_map (fun op -> match apply op v1 v2 with
            | None -> []
            | Some v -> [ (Binary (e1, op, e2), v) ]
          ) [Plus; Minus; Mult; Div]
        ) exprs2
      ) exprs1
    ) split_list

let equations (l : int list) : (exp * exp) list = match l with
  | [] -> failwith "Invalid input: empty list of numbers"
  | [_] -> failwith "Invalid input: only one number"
  | _ ->
    let split_list = splits l in
    List.concat_map (fun (l1, l2) ->
      let exprs1 = exprs l1 in
      let exprs2 = exprs l2 in
      List.concat_map (fun (e1, v1) ->
        List.filter_map (fun (e2, v2) ->
          if float_equal v1 v2 then Some (e1, e2) else None
        ) exprs2
      ) exprs1
    ) split_list

let puzzle (l : int list) : string list = List.map showEq (equations l)

