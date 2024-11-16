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

(* computing expressions, !! divide by 0 undefined *)
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
  | Const n -> string_of_int n (* const -> convert to string *)
  | Binary (e1, op, e2) ->
    let s1 = expToStr (precedence op) e1 in
    let s2 = expToStr ((precedence op) + 1) e2 in
    let s = s1 ^ opToStr op ^ s2 in
    addParen (p > precedence op) s





(* input int list gives exp list (in str format). *)
(* generate all possible expressions from the list, = sign at every index, then permutation for operators? seems like a lot *)
