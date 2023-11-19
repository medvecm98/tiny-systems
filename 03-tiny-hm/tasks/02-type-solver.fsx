// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  // TODO: Return true of type 'ty' contains variable 'vcheck'
  match ty with
  | TyVariable(vact) ->
    if vact = vcheck then true else false
  | TyList(ty) ->
    occursCheck vcheck ty
  | _ ->
    false
 
let rec substType (subst:Map<string, Type>) ty = 
  // TODO: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyVariable(tyVar) ->
    if Map.containsKey tyVar subst then subst.[tyVar] else ty
  | TyList(ty) ->
    TyList(substType subst ty)
  | _ ->
    ty

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
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

  // TODO: Fill in the remaining cases! You can closely follow the
  // example from task 1 - the logic here is exactly the same.


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
