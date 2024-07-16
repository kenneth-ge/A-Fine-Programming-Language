module StringMap = Map.Make(String)

(* affine = use at most once *)
(*type modifier = None | Affine*)
type exp = Nat of int | Neg of exp | Plus of exp * exp | Times of exp * exp | Minus of exp * exp
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

(* strict fixed point operator
 * Y = lambda G. (lambda g. G(lambda x. g g x)) (lambda g. G(lambda x. g g x))
 *)
let ggx = Lam ("g", App (X "G", Lam ("x", App (App (X "g", X "g"), X "x"))))
let z = Lam ("G", App (ggx, ggx))

let rec eval (env : value StringMap.t) = function 
        (Nat i) -> I i
    |   (Neg e) -> (match (eval env e) with
                    I i -> I (-i)
                |   _ -> raise TypeError)
    |   (Plus (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> I (i + i2)
            |   _ -> raise TypeError)
    |   (Times (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> I (i * i2)
            |   _ -> raise TypeError)
    |   (Minus (e1, e2)) -> 
        (match (eval env e1, eval env e2) with
            (I i, I i2) -> I (i - i2)
        |   _ -> raise TypeError)
    |   (Eq (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> (if i = i2 then true_ else false_)
            |   _ -> raise TypeError)
    |   (Less (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> (if i < i2 then true_ else false_)
            |   _ -> raise TypeError)
    |   (More (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> (if i > i2 then true_ else false_)
            |   _ -> raise TypeError)
    |   (X x) -> (print_keys env; print_string ("Finding: " ^ x ^ "\n"); flush stdout; StringMap.find x env)
    |   (App (e1, e2)) -> 
            let arg = eval env e2 in
            (* save the type environment only from the function, because the fn has not yet been fully evaluated *)
            let fn = eval env e1 in
            (match fn with
                Fun (closureenv, x, exp) -> 
                    let newgamma = StringMap.add x arg closureenv in
                    let () = print_string ("saved to " ^ x ^ "\n") in
                    eval newgamma exp
            |   _ -> raise TypeError)
    |   (Lam (x, exp)) -> Fun (env, x, exp)
    |   (Fix exp) -> eval env (App (z, exp))

let eval2 exp = 
    let ans = eval StringMap.empty exp in
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

let genif cond iftrue iffalse = 
    App(App (App (if_, cond), iftrue), iffalse)
let fact = App(z, (Lam ("f", Lam ("n", genif (Eq (X "n", Nat 0)) (Nat 1) (Times (X "n", App (X "f", Minus (X "n", Nat 1))))))))
(*let (I 1) = eval2 (App (fact, Nat 0)) *)