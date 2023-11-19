// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  // NOTE: Added three more kinds of expression from TinyML
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  // NOTE: Added type for functions (of single argument)
  | TyFunction of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable(vact) ->
    if vact = vcheck then true else false
  | TyFunction(ty1, ty2) ->
    occursCheck vcheck ty1 || occursCheck vcheck ty2
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
  | TyFunction(ty1, ty2) ->
    TyFunction(substType subst ty1, substType subst ty2)
  | _ ->
    ty

let substConstrs subst cs = 
  List.map (fun (t1, t2) -> (substType subst t1, substType subst t2)) cs
 
let rec solve cs =
  // TODO: Add case matching TyFunction(ta1, tb1) and TyFunction(ta2, tb2)
  // This generates two new constraints, equating the argument/return types.
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs | (TyBool, TyBool)::cs -> solve cs
  | (TyNumber, TyBool)::_   | (TyBool, TyNumber)::_ 
  | (TyList _, TyNumber)::_ | (TyNumber, TyList _)::_ 
  | (TyList _, TyBool)::_   | (TyBool, TyList _)::_ 
  | (TyList _, TyFunction _)::_ | (TyFunction _, TyList _)::_ 
  | (TyNumber, TyFunction _)::_ | (TyFunction _, TyNumber)::_ 
  | (TyBool, TyFunction _)::_ | (TyFunction _, TyBool)::_ 
    -> failwith("Cannot solve.")
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


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
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
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10))
|> infer 

// let f = fun x -> x*2 in (f 20) + (f 1)
Let("f",
  Lambda("x", Binary("*", Variable("x"), Constant(2))),
  Binary("+", 
    Application(Variable("f"), Constant(20)),
    Application(Variable("f"), Constant(1)) 
  ))
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", 
  Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

// fun f -> f f 
// This does not type check due to occurs check
Lambda("f", 
  Application(Variable "f", Variable "f"))
|> infer

fun f -> f 1 + f (2 = 3) 
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda("f", 
  Binary("+",
    Application(Variable "f", Constant 1),
    Application(Variable "f", Binary("=", Constant 2, Constant 3))
  ))
|> infer
