// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string


// NOTE: The four functions below currently return a wrong 
// result, but one that makes the code run. As you implement
// them (one by one), the tests should graudally start working.


let rec occursCheck (v:string) (n:Number) = 
  // TODO: Check if variable 'v' appears anywhere inside 'n'
  match n with
  | Variable(v2) ->
    if v = v2 then true else false
  | Succ(s) ->
    occursCheck v s
  | _ ->
    false


let rec substitute (v:string) (subst:Number) (n:Number) =
  // TODO: Replace all occurrences of variable 'v' in the
  // number 'n' with the replacement number 'subst'
  match n with
  | Variable(v2) ->
    if v = v2 then subst else n
  | Succ(s) ->
    Succ(substitute v subst s)
  | _ ->
    n

let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) = 
  // TODO: Substitute 'v' for 'subst' (use 'substitute') in 
  // all numbers in all the constraints in 'constraints'
  constraints |> List.map ( fun (n1, n2) -> (substitute v subst n1, substitute v subst n2) )


let substituteAll (subst:list<string * Number>) (n:Number) =
  // TODO: Perform all substitutions 
  // specified  in 'subst' on the number 'n'
  (n, subst) ||> List.fold (fun n1 (v, n2) -> substitute v n2 n1)

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
solve 
  [ Succ(Variable "x"), Succ(Zero) ]

solve 
  [ Succ(Zero), Succ(Variable "x") ]

// Should faild: S(Z) <> Z
solve 
  [ Succ(Succ(Zero)), Succ(Zero) ]

// Not done: Need to substitute x/Z in S(x)
solve 
  [ Succ(Variable "x"), Succ(Zero)
    Variable "y", Succ(Variable "x") ]

// Not done: Need to substitute z/Z in S(S(z))
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ]

// Not done: Need occurs check
solve
  [ Variable "x", Succ(Variable "x") ]
