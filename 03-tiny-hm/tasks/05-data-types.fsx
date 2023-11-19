// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with
  | TyVariable(vact) ->
    if vact = vcheck then true else false
  | TyFunction(ty1, ty2) | TyTuple(ty1, ty2) ->
    occursCheck vcheck ty1 || occursCheck vcheck ty2
  | TyList(ty) ->
    occursCheck vcheck ty
  | _ ->
    false

let rec substType (subst:Map<_, _>) ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with
  | TyVariable(tyVar) ->
    if Map.containsKey tyVar subst then subst.[tyVar] else ty
  | TyList(ty) ->
    TyList(substType subst ty)
  | TyFunction(ty1, ty2) ->
    TyFunction(substType subst ty1, substType subst ty2)
  | TyTuple(ty1, ty2) ->
    TyTuple(substType subst ty1, substType subst ty2)
  | _ ->
    ty

let substConstrs subst cs = 
  List.map (fun (t1, t2) -> (substType subst t1, substType subst t2)) cs
 
let rec solve cs =
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match cs with 
  | (TyNumber, TyBool)::_   | (TyBool, TyNumber)::_ 
  | (TyList _, TyNumber)::_ | (TyNumber, TyList _)::_ 
  | (TyList _, TyBool)::_   | (TyBool, TyList _)::_ 
  | (TyList _, TyFunction _)::_ | (TyFunction _, TyList _)::_ 
  | (TyNumber, TyFunction _)::_ | (TyFunction _, TyNumber)::_ 
  | (TyBool, TyFunction _)::_ | (TyFunction _, TyBool)::_ 
  | (TyList _, TyTuple _)::_ | (TyTuple _, TyList _)::_ 
  | (TyNumber, TyTuple _)::_ | (TyTuple _, TyNumber)::_ 
  | (TyBool, TyTuple _)::_ | (TyTuple _, TyBool)::_ 
  | (TyFunction _, TyTuple _)::_ | (TyTuple _, TyFunction _)::_ 
    -> failwith("Cannot solve.")
  | [] -> []
  | (TyNumber, TyNumber)::cs | (TyBool, TyBool)::cs -> solve cs
  | (TyList t1, TyList t2)::cs ->
    solve ((t1, t2)::cs)
  | (TyVariable var, ty)::cs | (ty, TyVariable var)::cs ->
    if occursCheck var ty then failwith "Cannot solve (occurs check)"
    let cs = substConstrs (Map.ofList [var, ty]) cs
    let subst = solve cs
    let ty = substType (Map.ofList subst) ty
    (var, ty)::subst
  | (TyFunction(tyF11, tyF12), TyFunction(tyF21, tyF22))::cs ->
    solve ((tyF11, tyF21)::(tyF12, tyF22)::cs)
  | (TyTuple(tyT11, tyT12), TyTuple(tyT21, tyT22))::cs ->
    solve ((tyT11, tyT21)::(tyT12, tyT22)::cs)


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

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

  | Binary("*", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

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

  | Let(v, e1, e2) ->
      // TODO: Generate type & constraints for 'e1' first and then
      // add the generated type to the typing context for 't2'.
      let t1, s1 = generate ctx e1
      let t2, s2 = generate (Map.add v t1 ctx) e2
      t2, s1 @ s2

  | Lambda(v, e) ->
      let targ = newTyVariable()
      // TODO: We do not know what the type of the variable 'v' is, so we 
      // generate a new type variable and add that to the 'ctx'. The
      // resulting type will be 'TyFunction' with 'targ' as argument type.
      let t1, s1 = generate (Map.add v targ ctx) e
      TyFunction(targ, t1), s1

  | Application(e1, e2) -> 
      // TODO: Tricky case! We cannot inspect the generated type of 'e1'
      // to see what the argument/return type of the function is. Instead,
      // we have to generate a new type variable and add a constraint.
      let t2, s2 = generate ctx e2
      let trv = newTyVariable()
      match e1 with
      | Variable _ ->
        let t1, s1 = generate ctx e1
        trv, s1 @ s2 @ [t1, TyFunction(t2, trv)]
      | _ -> failwith "a"

  | Tuple(e1, e2) ->
      // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2

  | TupleGet(b, e) ->
      // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
      // We need to generate two new type variables and a constraint.
      let tn1 = newTyVariable()
      let tn2 = newTyVariable()
      let t1, s1 = generate ctx e
      if b then
        tn1, s1 @ [t1, TyTuple(tn1, tn2)]
      else
        tn2, s1 @ [t1, TyTuple(tn1, tn2)]

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
