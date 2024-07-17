type datatype = UnitType | BoolType | IntType | FnType of datatype * datatype

(* Affine modifier = AtMost 1 *)
type modifier = NoMod | AtMost of int | AtLeast of int
let affine = AtMost 1

(* affine = use at most once *)
(*type modifier = None | Affine*)
type exp = Unit | Bool of bool
    | Nat of int | Neg of exp | Plus of exp * exp | Times of exp * exp | Minus of exp * exp | Div of exp * exp
    | Not of exp
    | Eq of exp * exp | Less of exp * exp | More of exp * exp
    | X of string | App of exp * exp | Lam of string * (datatype * modifier) * exp
    | Fix of exp
    | Let of string * (datatype * modifier) * exp * exp | If of exp * exp * exp