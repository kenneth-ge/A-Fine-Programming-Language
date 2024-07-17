(* Z combinator *)
(* plain Z combinator not well typed *)
(*val Z = fn f => ((fn x => f (fn z => x x z)) (fn x => f(fn z => x x z)))*)

datatype 'a fix = Fix of ('a fix -> 'a)
fun z f = 
  (fn (Fix x) => f (fn v => x (Fix x) v)) 
  (Fix (fn (Fix x) => f (fn v => x (Fix x) v)))