type value =
  | Var of string

type exp =
  | Val of value
  | Assign of value * exp
  | Eval of exp * exp
  | Alias of string * exp