module StringMap = Map.Make(String)

exception VarNotFound
exception TypeError
exception TypeCheckError of string

(*  Adding BoolType and first-class if just makes it easier to avoid mistakes vs using 
    the if combinator

    Going from lower level to higher level!!!
*)
type datatype = UnitType | BoolType | IntType | FnType of datatype * datatype

(* affine = use at most once *)
(*type modifier = None | Affine*)
type exp = Unit
    | Nat of int | Neg of exp | Plus of exp * exp | Times of exp * exp | Minus of exp * exp
    | Not of exp
    | Eq of exp * exp | Less of exp * exp | More of exp * exp
    | X of string | App of exp * exp | Lam of string * datatype * exp
    | Fix of exp
    | Let of string * exp * exp | If of exp * exp * exp

type value = UnitVal | Bool of bool | I of int | Fun of envir * string * exp
and envir = value StringMap.t

(* strict fixed point operator
 * Y = lambda G. (lambda g. G(lambda x. g g x)) (lambda g. G(lambda x. g g x))
 *)
let ggx = Lam ("g", UnitType, App (X "G", Lam ("x", UnitType, App (App (X "g", X "g"), X "x"))))
let z = Lam ("G", UnitType, App (ggx, ggx))

let rec eval (env : value StringMap.t) = function 
        Unit -> UnitVal
    |   Not b -> (match (eval env b) with
            Bool b -> Bool (not b)
        |   _ -> raise TypeError)
    |   (Nat i) -> I i
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
                (I i, I i2) -> (if i = i2 then Bool true else Bool false)
            |   _ -> raise TypeError)
    |   (Less (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> (if i < i2 then Bool true else Bool false)
            |   _ -> raise TypeError)
    |   (More (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> (if i > i2 then Bool true else Bool false)
            |   _ -> raise TypeError)
    |   (X x) -> StringMap.find x env
    |   (App (e1, e2)) -> 
            let arg = eval env e2 in
            (* save the type environment only from the function, because the fn has not yet been fully evaluated *)
            let fn = eval env e1 in
            (match fn with
                Fun (closureenv, x, exp) -> 
                    let env' = StringMap.add x arg closureenv in
                    (*let () = print_string ("saved to " ^ x ^ "\n") in*)
                    eval env' exp
            |   _ -> raise TypeError)
    |   (Lam (x, datatype, exp)) -> Fun (env, x, exp)
    |   (Fix exp) -> 
            eval env (App (z, exp))
            (*(match eval env exp with
                (Fun (closureenv, varname, body)) as f -> 
                    let closureenv' = StringMap.add varname (eval closureenv (Lam ("()", UnitType, Fix exp))) closureenv in
                    Fun (closureenv', varname, body)
            |   _ -> raise TypeError
            )*)
    |   (Let (name, value, next)) -> 
            let newenv = StringMap.add name (eval env value) env in
            eval newenv next
    |   (If (cond, succ, fail)) ->
            if (eval env cond) = (Bool true) then eval env succ else eval env fail

let rec typecheck (gamma: datatype StringMap.t) = function
        Unit -> UnitType
    |   Not e -> (match typecheck gamma e with
                    BoolType -> BoolType
                |   _ -> raise (TypeCheckError "Can only take not of bool"))
    |   (Nat i) -> IntType
    |   (Neg e) -> (match (typecheck gamma e) with
                    IntType -> IntType
                |   _ -> raise (TypeCheckError "Can only take negative of int"))
    |   (Plus (e1, e2))
    |   (Times (e1, e2))
    |   (Minus (e1, e2)) -> 
        (match (typecheck gamma e1, typecheck gamma e2) with
            (IntType, IntType) -> IntType
        |   _ -> raise (TypeCheckError "Arithmetic operator args need to be ints"))
    |   (Eq (e1, e2)) -> 
            (match (typecheck gamma e1, typecheck gamma e2) with
                (IntType, IntType) -> BoolType
            |   (BoolType, BoolType) -> BoolType
            |   (UnitType, UnitType) -> BoolType
            |   _ -> raise (TypeCheckError "can only compare ints, bools, and unit types using = operator"))
    |   (Less (e1, e2))
    |   (More (e1, e2)) -> 
            (match (typecheck gamma e1, typecheck gamma e2) with
                (IntType, IntType) -> BoolType
            |   _ -> raise (TypeCheckError "can only compare ints using <> operators"))
    |   (X x) -> (StringMap.find x gamma)
    |   (App (e1, e2)) -> 
            let arg = typecheck gamma e2 in
            (* save the type environment only from the function, because the fn has not yet been fully evaluated *)
            let fn = typecheck gamma e1 in
            (match fn with
                FnType (input, output) -> 
                    if arg = input then output else raise (TypeCheckError "Argument has different type than what the function wants")
            |   _ -> raise (TypeCheckError "Cannot apply when fn is not of function type"))
    |   (Lam (x, datatype, exp)) -> (
            let gamma' = StringMap.add x datatype gamma in
            let bodytype = typecheck gamma' exp in
            FnType (datatype, bodytype)
        )
    |   (Fix exp) -> (* fix has type (('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) *)
             (match typecheck gamma exp with
                FnType (FnType (a, b), FnType (c, d)) -> if a = c && b = d then FnType (a, b) else (raise (TypeCheckError "You have a function of the form ('a -> 'b) -> 'c -> 'd, where either 'a != 'c or 'b != 'd. But, we should have 'a = 'c and 'b = 'd"))
            |   _ -> raise (TypeCheckError "Can only fix fn of type ('a -> 'b) -> 'a -> 'b")
             )
    |   (Let (name, value, next)) -> 
            let gamma' = StringMap.add name (typecheck gamma value) gamma in
            typecheck gamma' next
    |   (If (arg, iftrue, iffalse)) -> 
            match (typecheck gamma arg, typecheck gamma iftrue, typecheck gamma iffalse) with
                (BoolType, a, b) -> if a = b then a else raise (TypeCheckError "branches of if statement don't match")
            |   _ -> raise (TypeCheckError "if statement needs to take in bool")

let eval2 exp =
    let typecheckresult = typecheck StringMap.empty exp in 
    let () = print_string "type checked successfully! \n"; flush stdout in
    let ans = eval StringMap.empty exp in
    ans

let nattest = Nat 5
let (I 5) = eval2 nattest

let negtest = Neg nattest
let (I (-5)) = eval2 negtest
let plustest = Plus (negtest, nattest)
let (I 0) = eval2 plustest

let plusonefn = Lam ("x", IntType, Plus (X "x", Nat 1))
(* REPL command: #use "start.ml";;*)
let (I 5) = eval2 (App (plusonefn, Nat 4))

let currytest = Lam ("input", IntType, Lam ("transform", FnType (IntType, IntType), App (X "transform", X "input")))
let (I 5) = eval2 (App (App (currytest, Nat 4), plusonefn))

let iftest = Nat 5 = If(Eq (Nat 3, Nat 3), Nat 5, Nat 6)

let k = Lam ("f", FnType (UnitType, IntType), Lam ("()", UnitType, Nat 1))
let test = Fix (k)
let I 1 = eval2 (App (test, Unit))

let innerfact = Lam ("f", FnType (IntType, IntType), Lam ("n", IntType, If (Eq (X "n", Nat 0), Nat 1, Times (X "n", App (X "f", Minus (X "n", Nat 1))))))
let fact = Fix(innerfact)
let (I 1) = eval2 (App (fact, Nat 0))
let (I 1) = eval2 (App (fact, Nat 1))
let (I 2) = eval2 (App (fact, Nat 2))
let (I 6) = eval2 (App (fact, Nat 3))
let (I 24) = eval2 (App (fact, Nat 4))
let (I 120) = eval2 (App (fact, Nat 5))

(* test let statements *)
let (I 720) = eval2 (
    Let ("fact_internal", innerfact,
        Let ("fact", Fix ((X "fact_internal")),
        App (X "fact", Nat 6))
    ))