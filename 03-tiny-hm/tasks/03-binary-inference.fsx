// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

// NOTE: Start with some basic expressions from TinyML
// This time, If requires a real Boolean argument and we have
// operators '+' (int -> int -> int) and '=' (int -> int -> bool)
type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable(vact) ->
    if vact = vcheck then true else false
  | TyList(ty) ->
    occursCheck vcheck ty
  | _ ->
    false

let rec substType (subst:Map<_, _>) ty = 
  match ty with
  | TyVariable(tyVar) ->
    if Map.containsKey tyVar subst then subst.[tyVar] else ty
  | TyList(ty) ->
    TyList(substType subst ty)
  | _ ->
    ty

let substConstrs subst cs = 
  List.map (fun (t1, t2) -> (substType subst t1, substType subst t2)) cs
 
let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs | (TyBool, TyBool)::cs -> solve cs
  | (TyNumber, TyBool)::_   | (TyBool, TyNumber)::_ 
  | (TyList _, TyNumber)::_ | (TyNumber, TyList _)::_ 
  | (TyList _, TyBool)::_   | (TyBool, TyList _)::_ ->
    failwith("Cannot solve.")
  | (TyList t1, TyList t2)::cs ->
    solve ((t1, t2)::cs)
  | (TyVariable var, ty)::cs | (ty, TyVariable var)::cs ->
    if occursCheck var ty then failwith "Cannot solve (occurs check)"
    let cs = substConstrs (Map.ofList [var, ty]) cs
    let subst = solve cs
    let ty = substType (Map.ofList subst) ty
    (var, ty)::subst

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      // TODO: Similar to the case for '+' but returns 'TyBool'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, t2 ]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      // TODO: Just get the type of the variable from 'ctx' here.
      ctx.[v], []

  | If(econd, etrue, efalse) ->
      // TODO: Call generate recursively on all three sub-expressions,
      // collect all constraints and add a constraint that (i) the type
      // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
      let t1, s1 = generate ctx econd
      let t2, s2 = generate ctx etrue
      let t3, s3 = generate ctx efalse
      t2, s1 @ s2 @ s3 @ [t1, TyBool; t2, t3]


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = 
  Binary("=",   
    Variable("x"), 
    Binary("+", Constant(10), Variable("x")))

let t1, cs1 = 
  generate (Map.ofList ["x", TyVariable "a"]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("y"))

let t2, cs2 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("x"))

let t3, cs3 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e3

solve cs3
