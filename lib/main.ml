(* defining operation as the 4 common operators *)
type op =
  | Plus
  | Minus
  | Multiply
  | Divide

(* exp as a constant or as a binary operation *)
type exp =
  | Const of int
  | Binary of exp * op * exp

(* computing expressions, !! divide by 0 undefined *)
let apply (op : op) (x : float) (y : float) : float option = match op with
  | Plus -> Some (x +. y)
  | Minus -> Some (x -. y)
  | Multiply -> Some (x *. y)
  | Divide when y = 0.0 -> None
  | Divide -> Some (x /. y)

