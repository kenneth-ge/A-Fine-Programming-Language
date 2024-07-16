module StringMap = Map.Make(String)

(* affine = use at most once *)
(*type modifier = None | Affine*)
type exp = Nat of int | Neg of exp | Plus of exp * exp 
    | Eq of exp * exp | Less of exp * exp | More of exp * exp
    | X of string | App of exp * exp | Lam of string * exp
    | Fix of exp

exception VarNotFound
exception TypeError

type value = I of int | Fun of envir * string * exp
and envir = value StringMap.t

let print_keys (m : value StringMap.t) = 
    let () = StringMap.iter (fun key -> fun value -> print_string ("Key: " ^ key ^ "\n")) m in
    flush stdout

let if_ = Lam ("Conditional", Lam ("A", Lam ("B", App (App (X "Conditional", X "A"), X "B"))))

let selectfirst = Fun (StringMap.empty, "A", Lam ("B", X "A"))
let selectsecond = Fun (StringMap.empty, "A", Lam ("B", X "B"))
let true_ = selectfirst
let false_ = selectsecond

let rec eval (env : value StringMap.t) = function 
        (Nat i) -> (I i, env)
    |   (Neg e) -> (match (eval env e) with
                    (I i, _) -> (I (-i), env)
                |   _ -> raise TypeError)
    |   (Plus (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                ((I i, _), (I i2, _)) -> (I (i + i2), env)
            |   _ -> raise TypeError)
    |   (Eq (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                ((I i, _), (I i2, _)) -> ((if i = i2 then true_ else false_), env)
            |   _ -> raise TypeError)
    |   (Less (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                ((I i, _), (I i2, _)) -> ((if i = i2 then true_ else false_), env)
            |   _ -> raise TypeError)
    |   (More (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                ((I i, _), (I i2, _)) -> ((if i = i2 then true_ else false_), env)
            |   _ -> raise TypeError)
    |   (X x) -> (print_keys env; print_string ("Finding: " ^ x ^ "\n"); flush stdout; (StringMap.find x env, env))
    |   (App (e1, e2)) -> 
            let arg, _ = eval env e2 in
            (* save the type environment only from the function, because the fn has not yet been fully evaluated *)
            let fn, gamma2 = eval env e1 in
            (match fn with
                Fun (x, exp) -> 
                    let newgamma = StringMap.add x arg gamma2 in
                    let () = print_string ("saved to " ^ x ^ "\n") in
                    eval newgamma exp
            |   _ -> raise TypeError)
    |   (Lam (x, exp)) -> (Fun (x, exp), env)
    |   (Fix exp) -> (match eval env exp with
        (* fixed point primitive *)
            Fun (x, exp), env -> (

            )
        |   _ -> raise TypeError
        )

let eval2 exp = 
    let (ans, gamma) = eval StringMap.empty exp in
    ans

let nattest = Nat 5
let (I 5) = eval2 nattest

let negtest = Neg nattest
let (I (-5)) = eval2 negtest
let plustest = Plus (negtest, nattest)
let (I 0) = eval2 plustest

let plusonefn = Lam ("x", Plus (X "x", Nat 1))
(* REPL command: #use "start.ml";;*)
let (I 5) = eval2 (App (plusonefn, Nat 4))

let currytest = Lam ("input", Lam ("transform", App (X "transform", X "input")))
let (I 5) = eval2 (App (App (currytest, Nat 4), plusonefn))

let iftest = Nat 5 = App (App (App (if_, Eq (Nat 3, Nat 3)), Nat 5), Nat 6)