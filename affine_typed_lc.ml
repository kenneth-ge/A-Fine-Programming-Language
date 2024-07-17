type datatype = UnitType | BoolType | IntType | FnType of datatype * datatype

(* Affine modifier = AtMost 1 *)
type modifier = NoMod | AtMost of int | AtLeast of int
let affine = AtMost 1

(* affine = use at most once *)
(*type modifier = None | Affine*)
type exp = Unit | Bool of bool | Nat of int 
    | Neg of exp | Not of exp 
    | Or of exp * exp | And of exp * exp
    | Plus of exp * exp | Times of exp * exp | Minus of exp * exp | Div of exp * exp
    | Eq of exp * exp | Less of exp * exp | More of exp * exp
    | X of string | App of exp * exp | Lam of string * (datatype * modifier) * exp
    | Fix of exp
    | Let of string * (datatype * modifier) * exp * exp | If of exp * exp * exp

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

exception VarNotFound
exception TypeError
exception TypeCheckError of string

(*  Adding BoolType and first-class if just makes it easier to avoid mistakes vs using 
    the if combinator

    Going from lower level to higher level!!!
*)

type value = UnitVal | Bool of bool | I of int | Fun of envir * string * exp
and envir = value StringMap.t

(* strict fixed point operator
 * Y = lambda G. (lambda g. G(lambda x. g g x)) (lambda g. G(lambda x. g g x))
 *)
let ggx = Lam ("g", (UnitType, NoMod), App (X "G", Lam ("x", (UnitType, NoMod), App (App (X "g", X "g"), X "x"))))
let z = Lam ("G", (UnitType, NoMod), App (ggx, ggx))

let rec eval (env : envir) = function 
        Unit -> UnitVal
    |   Bool b -> Bool b
    |   Not b -> (match (eval env b) with
            Bool b -> Bool (not b)
        |   _ -> raise TypeError)
    |   (Nat i) -> I i
    |   (Neg e) -> (match (eval env e) with
                    I i -> I (-i)
                |   _ -> raise TypeError)
    |   (Or (e1, e2)) -> 
        (match (eval env e1, eval env e2) with
            (Bool b, Bool b2) -> Bool (b || b2)
        |   _ -> raise TypeError)
    |   (And (e1, e2)) -> 
        (match (eval env e1, eval env e2) with
            (Bool b, Bool b2) -> Bool (b && b2)
        |   _ -> raise TypeError)
    |   (Plus (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> I (i + i2)
            |   _ -> raise TypeError)
    |   (Times (e1, e2)) -> 
            (match (eval env e1, eval env e2) with
                (I i, I i2) -> I (i * i2)
            |   _ -> raise TypeError)
    |   (Div (e1, e2)) -> 
        (match (eval env e1, eval env e2) with
            (I i, I i2) -> I (i / i2)
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
    |   (Lam (x, datatypeandmod, exp)) -> Fun (env, x, exp)
    |   (Fix exp) -> 
            eval env (App (z, exp))
            (*(match eval env exp with
                (Fun (closureenv, varname, body)) as f -> 
                    let closureenv' = StringMap.add varname (eval closureenv (Lam ("()", UnitType, Fix exp))) closureenv in
                    Fun (closureenv', varname, body)
            |   _ -> raise TypeError
            )*)
    |   (Let (name, _, value, next)) -> 
            let newenv = StringMap.add name (eval env value) env in
            eval newenv next
    |   (If (cond, succ, fail)) ->
            if (eval env cond) = (Bool true) then eval env succ else eval env fail

let rec removeShadowed name = function
    [] -> []
|   (n, v) :: xs -> if n = name then removeShadowed name xs else (n, v) :: removeShadowed name xs

let processChanges name changesList originalModifier =
    let rec countUses = function
        [] -> 0, []
    |   (varname, diff) as curr :: xs -> (
            let changes, remainder = countUses xs in
            (if name = varname then (diff + changes, remainder) else (changes, curr :: remainder))) in
    let varUses, leftover = countUses changesList in
    let checkMods = (match originalModifier with
        NoMod -> leftover
    |   AtMost k -> if varUses > k then 
                raise (TypeCheckError ("Affine type used too many times " ^ string_of_int varUses ^ " > " ^ string_of_int k)) 
                else leftover
    |   AtLeast k -> if varUses < k then raise (TypeCheckError ("Affine type not used enough times " ^ string_of_int varUses ^ " < " ^ string_of_int k)) else leftover) in
    removeShadowed name checkMods

let rec countsame name = function
        [] -> 0, []
    |   (x, stuff) :: xs -> 
            let cnt, rest = countsame name xs in
            if x = name then (cnt + stuff), rest else cnt, (x, stuff) :: rest

let rec sumindiv = function
        [] -> []
    |   (name, diff) :: xs -> 
        let countthisone, leftovers = countsame name xs in
        (name, diff + countthisone) :: (sumindiv leftovers)

let rec pairwisemax l1 = function
        [] -> l1
    |   (n, cnt) :: xs -> 
        match List.find_opt (fun (n2, cnt2) -> n = n2) l1 with
            None -> (n, cnt) :: pairwisemax l1 xs
        |   Some (_, cnt2) -> (n, max cnt cnt2) :: pairwisemax (List.remove_assoc n l1) xs
            

let rec consolidate acc = function
        [] -> acc
    |   x :: xs ->
        let processed = sumindiv x in
        consolidate (pairwisemax acc processed) xs

let rec print_changes = function
    [] -> print_string "]\n"; flush stdout
|   (x, cnt) :: xs -> print_string (x ^ " " ^ string_of_int cnt ^ " "); print_changes xs

let rec typecheck (gamma: datatype StringMap.t) = function
        Unit -> UnitType, []
    |   Bool b -> BoolType, []
    |   Not e -> (match typecheck gamma e with
                    BoolType, c -> BoolType, c
                |   _ -> raise (TypeCheckError "Can only take not of bool"))
    |   (Nat i) -> IntType, []
    |   (Neg e) -> (match (typecheck gamma e) with
                    IntType, c -> IntType, c
                |   _ -> raise (TypeCheckError "Can only take negative of int"))
    |   (Or (e1, e2))
    |   (And (e1, e2)) -> (* These are examples of x operators *)
        (match (typecheck gamma e1, typecheck gamma e2) with
            ((BoolType, c), (BoolType, c2)) -> BoolType, (c @ c2)
        |   _ -> raise (TypeCheckError "And/or operator args need to be bools"))
    |   (Plus (e1, e2))
    |   (Times (e1, e2))
    |   (Div (e1, e2))
    |   (Minus (e1, e2)) -> (* These are examples of x operators *)
        (match (typecheck gamma e1, typecheck gamma e2) with
            ((IntType, c), (IntType, c2)) -> IntType, (c @ c2)
        |   _ -> raise (TypeCheckError "Arithmetic operator args need to be ints"))
    |   (Eq (e1, e2)) -> 
            (match (typecheck gamma e1, typecheck gamma e2) with
                ((IntType, c), (IntType, c2)) -> BoolType, c @ c2
            |   ((BoolType, c), (BoolType, c2)) -> BoolType, c @ c2
            |   ((UnitType, c), (UnitType, c2)) -> BoolType, c @ c2
            |   _ -> raise (TypeCheckError "can only compare ints, bools, and unit types using = operator"))
    |   (Less (e1, e2))
    |   (More (e1, e2)) -> 
            (match (typecheck gamma e1, typecheck gamma e2) with
                ((IntType, c), (IntType, c2)) -> BoolType, c @ c2
            |   _ -> raise (TypeCheckError "can only compare ints using <> operators"))
    |   (X x) -> 
            let t = StringMap.find x gamma in
            t, [(x, 1)]
    |   (App (e1, e2)) -> 
            let argT, ch = typecheck gamma e2 in
            (* save the type environment only from the function, because the fn has not yet been fully evaluated *)
            let fnT, ch2 = typecheck gamma e1 in
            (match fnT with
                FnType (inputT, outputT) -> 
                    if argT = inputT then outputT, ch @ ch2 else raise (TypeCheckError "Argument has different type than what the function wants")
            |   _ -> raise (TypeCheckError "Cannot apply when fn is not of function type"))
    |   (Lam (x, (datatype, modif), exp)) -> (
            (*  All affine types begin here, because this is where we name 
                variables and can specify that a type needs to be affine *)
            let gamma' = StringMap.add x datatype gamma in
            let bodytype, changes = typecheck gamma' exp in
            let remainingChanges = processChanges x changes modif in
            FnType (datatype, bodytype), remainingChanges
        )
    |   (Fix exp) -> (* fix has type (('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) *)
             (match typecheck gamma exp with
                (FnType (FnType (a, b), FnType (c, d)), changes) -> if a = c && b = d then FnType (a, b), changes else (raise (TypeCheckError "You have a function of the form ('a -> 'b) -> 'c -> 'd, where either 'a != 'c or 'b != 'd. But, we should have 'a = 'c and 'b = 'd"))
            |   _ -> raise (TypeCheckError "Can only fix fn of type ('a -> 'b) -> 'a -> 'b")
             )
    |   (Let (name, (decltype, modif), value, next)) -> 
            (* Or they begin here 
               Note that the value in the expression for the `let` statement
               cannot use the same name that we are doing in the current let statement
               and stuff (unless it's shadowing)
            *)
            let t, modlist = typecheck gamma value in
            let modlist = removeShadowed name modlist in
            let () = if decltype = t then () else raise (TypeCheckError "Declared type in let statement different from actual type of expression") in
            let gamma' = StringMap.add name t gamma in
            let bodytype, changes = typecheck gamma' next in
            let remainingChanges = processChanges name (modlist @ changes) modif in
            bodytype, remainingChanges
    |   (If (arg, iftrue, iffalse)) ->
            (* If is a special case where instead of just adding changes together, we take the max *)
            match (typecheck gamma arg, typecheck gamma iftrue, typecheck gamma iffalse) with
                ((BoolType, changes1), (a, changes2), (b, changes3)) -> if a = b then a, changes1 @ (consolidate [] [changes2; changes3]) else raise (TypeCheckError "branches of if statement don't match")
            |   _ -> raise (TypeCheckError "if statement needs to take in bool")

let eval2 exp =
    let _ = typecheck StringMap.empty exp in 
    let () = print_string "type checked successfully! \n"; flush stdout in
    let ans = eval StringMap.empty exp in
    ans

(* 
print_changes changes1; print_changes changes2; print_changes changes3; print_string "["; print_changes (changes1 @ changes2); print_string "] consolidated:"; print_changes (changes1 @ (consolidate [] [changes2; changes3])); flush stdout; 

let nattest = Nat 5
let (I 5) = eval2 nattest

let negtest = Neg nattest
let (I (-5)) = eval2 negtest
let plustest = Plus (negtest, nattest)
let (I 0) = eval2 plustest

let plusonefn = Lam ("x", (IntType, NoMod), Plus (X "x", Nat 1))
(* REPL command: #use "start.ml";;*)
let (I 5) = eval2 (App (plusonefn, Nat 4))

let currytest = Lam ("input", (IntType, NoMod), Lam ("transform", (FnType (IntType, IntType), NoMod), App (X "transform", X "input")))
let (I 5) = eval2 (App (App (currytest, Nat 4), plusonefn))

let iftest = Nat 5 = If(Eq (Nat 3, Nat 3), Nat 5, Nat 6)

let k = Lam ("f", (FnType (UnitType, IntType), NoMod), Lam ("()", (UnitType, NoMod), Nat 1))
let test = Fix (k)
let I 1 = eval2 (App (test, Unit))

let innerfact = Lam ("f", (FnType (IntType, IntType), NoMod), Lam ("n", (IntType, NoMod), If (Eq (X "n", Nat 0), Nat 1, Times (X "n", App (X "f", Minus (X "n", Nat 1))))))
let fact = Fix(innerfact)
let (I 1) = eval2 (App (fact, Nat 0))
let (I 1) = eval2 (App (fact, Nat 1))
let (I 2) = eval2 (App (fact, Nat 2))
let (I 6) = eval2 (App (fact, Nat 3))
let (I 24) = eval2 (App (fact, Nat 4))
let (I 120) = eval2 (App (fact, Nat 5))

(* test let statements *)
let (I 720) = eval2 (
    Let ("fact_internal", (FnType (FnType (IntType, IntType), FnType (IntType, IntType)), NoMod), innerfact,
        Let ("fact", (FnType (IntType, IntType), NoMod), Fix (X "fact_internal"),
        App (X "fact", Nat 6))
    ))*)

let rec string_of_exp = function
    | Unit -> "Unit"
    | Bool b -> string_of_bool b
    | Nat n -> string_of_int n
    | Neg e -> "Neg(" ^ string_of_exp e ^ ")"
    | And (e1, e2) -> "And(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Or (e1, e2) -> "Or(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Plus (e1, e2) -> "Plus(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Times (e1, e2) -> "Times(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Minus (e1, e2) -> "Minus(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Div (e1, e2) -> "Div(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Not e -> "Not(" ^ string_of_exp e ^ ")"
    | Eq (e1, e2) -> "Eq(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Less (e1, e2) -> "Less(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | More (e1, e2) -> "More(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | X x -> "X(" ^ x ^ ")"
    | App (e1, e2) -> "App(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | Lam (x, (typ, modif), e) -> "Lam(" ^ x ^ ", (" ^ string_of_typ typ ^ "," ^ string_of_mod modif ^ "), " ^ string_of_exp e ^ ")"
    | Fix e -> "Fix(" ^ string_of_exp e ^ ")"
    | Let (x, (typ, _), e1, e2) -> "Let(" ^ x ^ ", " ^ string_of_typ typ ^ ", " ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
    | If (e1, e2, e3) -> "If(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ", " ^ string_of_exp e3 ^ ")"

and string_of_typ = function
    | IntType -> "IntType"
    | BoolType -> "BoolType"
    | UnitType -> "UnitType"
    | FnType (t1, t2) -> "FunctionType(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
and string_of_mod = function
    | NoMod -> "NoMod"
    | AtLeast x -> "AtLeast " ^ string_of_int x
    | AtMost x -> "AtMost " ^ string_of_int x

let rec string_of_value = function
    | UnitVal -> "()"
    | Bool b -> string_of_bool b
    | I i -> string_of_int i
    | Fun (_, s, exp) -> "[...] fn " ^ s ^ " => " ^ string_of_exp exp

(*let () = print_string (string_of_value (eval2 (App (Lam ("x", (IntType, NoMod), Plus (X "x", Nat 1)), Nat 4))))*)
