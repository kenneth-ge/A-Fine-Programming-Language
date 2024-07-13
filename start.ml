module StringMap = Map.Make(String)

type t = Int | To of t * t
(* affine = use at most once *)
(*type modifier = None | Affine*)
type exp = Nat of int | Neg of exp | 
    Plus of exp * exp | X of string | App of exp * exp | Lam of string * t * exp

exception VarNotFound
exception TypeError

type basecase = I of int | Fun of string * t * exp

let print_keys (m : basecase StringMap.t) = 
    let () = StringMap.iter (fun key -> fun value -> print_string ("Key: " ^ key ^ "\n")) m in
    flush stdout

let rec eval (gamma : basecase StringMap.t) = function 
        (Nat i) -> (I i, gamma)
    |   (Neg e) -> (match (eval gamma e) with
                    (I i, _) -> (I (-i), gamma)
                |   _ -> raise TypeError)
    |   (Plus (e1, e2)) -> 
            (match (eval gamma e1, eval gamma e2) with
                ((I i, _), (I i2, _)) -> (I (i + i2), gamma)
            |   _ -> raise TypeError)
    |   (X x) -> (print_keys gamma; print_string ("Finding: " ^ x ^ "\n"); flush stdout; (StringMap.find x gamma, gamma))
    |   (App (e1, e2)) -> 
            let arg, _ = eval gamma e2 in
            let fn, gamma2 = eval gamma e1 in
            (match fn with
                Fun (x, t, exp) -> 
                    let newgamma = StringMap.add x arg gamma2 in
                    let () = print_string ("saved to " ^ x ^ "\n") in
                    eval newgamma exp
            |   _ -> raise TypeError)
    |   (Lam (x, t, exp)) -> (Fun (x, t, exp), gamma)

let eval2 exp = 
    let (ans, gamma) = eval StringMap.empty exp in
    ans

let nattest = Nat 5
let (I 5) = eval2 nattest

let negtest = Neg nattest
let (I (-5)) = eval2 negtest
let plustest = Plus (negtest, nattest)
let (I 0) = eval2 plustest

let plusonefn = Lam ("x", Int, Plus (X "x", Nat 1))
(* REPL command: #use "start.ml";;*)
let (I 5) = eval2 (App (plusonefn, Nat 4))

let currytest = Lam ("input", Int, Lam ("transform", To (Int, Int), App (X "transform", X "input")))
let (I 5) = eval2 (App (App (currytest, Nat 4), plusonefn))