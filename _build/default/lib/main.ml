(* source code here *)
type expr =
  | Const of int
  | Binary of expr * op * expr
and op =
  | Plus
  | Minus
  | Multiply
  | Divide

type value = float

let op_name (op : op) : string =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Divide -> "/"

let op_prec (op : op) : int =
  match op with
  | Plus | Minus -> 6
  | Multiply | Divide -> 7

let rec show_expr (p : int) (e : expr) : string =
  match e with
  | Const n -> string_of_int n
  | Binary (e1, op, e2) ->
      let prec = op_prec op in
      let left = show_expr prec e1 in
      let right = show_expr (prec + 1) e2 in
      if p > prec then
        "(" ^ left ^ op_name op ^ right ^ ")"
      else
        left ^ op_name op ^ right

let show_equation (eq : expr * expr) : string =
  let (l, r) = eq in
  show_expr 0 l ^ " = " ^ show_expr 0 r

(* applying an operator to arguments may fail (division by zero) *)
let apply (op : op) (x : value) (y : value) : value option =
  match op with
  | Plus -> Some (x +. y)
  | Minus -> Some (x -. y)
  | Multiply -> Some (x *. y)
  | Divide when y = 0.0 -> None
  | Divide -> Some (x /. y)

(* e1 op (e2 op' e3) == (e1 op e2) op' e3 *)
let right_associative (op : op) (e : expr) : bool =
  match op, e with
  | (Plus, Binary (_, Plus, _)) -> true
  | (Plus, Binary (_, Minus, _)) -> true
  | (Multiply, Binary (_, Multiply, _)) -> true
  | (Multiply, Binary (_, Divide, _)) -> true
  | _ -> false

(* splittings of a list into two non-empty lists *)
let splits (xs : 'a list) : ('a list * 'a list) list =
  let rec aux l r = match l with
    | [] -> []
    | [x] -> [(r, [x])]
    | x :: xs -> (r, x :: xs) :: aux xs (x :: r)
  in aux xs []

(* generate all expressions from the numbers, except those containing a division by zero, or redundant right-associativity *)
let rec exprs (ns : int list) : (expr * value) list =
  match ns with
  | [n] -> [(Const n, float_of_int n)]
  | _ ->
      List.concat_map (fun (ns1, ns2) ->
        List.concat_map (fun (e1, v1) ->
          List.concat_map (fun (e2, v2) ->
            List.concat_map (fun op ->
              if right_associative op e2 then []
              else match apply op v1 v2 with
                | Some v -> [(Binary (e1, op, e2), v)]
                | None -> []
            ) [Plus; Minus; Multiply; Divide]
          ) (exprs ns2)
        ) (exprs ns1)
      ) (splits ns)

(* generate all correct equations from the list of numbers *)
let equations (ns : int list) : (expr * expr) list =
  match ns with
  | [] -> invalid_arg "empty list of numbers"
  | [_] -> invalid_arg "only one number"
  | _ ->
      List.concat_map (fun (ns1, ns2) ->
        List.concat_map (fun (e1, v1) ->
          List.concat_map (fun (e2, v2) ->
            if v1 = v2 then [(e1, e2)] else []
          ) (exprs ns2)
        ) (exprs ns1)
      ) (splits ns)

(* top-level function: all correct equations generated from the list of numbers, as pretty strings *)
let puzzle (ns : int list) : string list =
  List.map show_equation (equations ns)

(* Example usage: *)
let result = puzzle [1; 2; 3];;
