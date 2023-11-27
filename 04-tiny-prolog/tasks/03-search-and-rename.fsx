// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with
  | Variable v ->
    if Map.containsKey v subst then
      subst.[v]
    else
      Variable v
  | Predicate (p, term::terms) ->
    Predicate (p, (substitute subst term)::(substituteTerms subst terms))
  | t -> t
and substituteTerms subst (terms:list<Term>) = 
  match terms with
  | term::terms ->
    (substitute subst term)::(substituteTerms subst terms)
  | [] ->
    []

let append (m1:Map<_,_>) m2 =
  m1 |> Seq.fold(fun st (KeyValue(k, v)) -> Map.add k v st) m2

let rec substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  match subst with
  | (var, term)::ls ->
    (var, substitute newSubst term)::(substituteSubst newSubst ls)
  | [] -> []

let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] -> 
      Some []
  | h1::t1, h2::t2 -> 
      let simpleUni = unify h1 h2
      match simpleUni with
      | Some(simpleUni) ->
        let t1Subst = substituteTerms (Map.ofList simpleUni) t1
        let t2Subst = substituteTerms (Map.ofList simpleUni) t2
        let listUni = unifyLists t1Subst t2Subst
        match listUni with 
        | Some(lu) -> Some(lu @ (substituteSubst (Map.ofList lu) simpleUni))
        | _ -> None
      | _ -> None
  | _ -> 
    None

and unify t1 t2 = 
  match t1, t2 with 
  | Atom s1 , Atom s2 ->
    if s1 = s2 then Some [] else None
  | t, Variable v | Variable v, t ->
    Some [(v, t)]
  | Predicate (p1, terms1), Predicate (p2, terms2) ->
    if p1 = p2 then
      unifyLists terms1 terms2
    else
      None
  | _ ->
      None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  // TODO: Return a list of all variables that appear in 'term'
  // (this may contain duplicates, we will eliminate them below)
  // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
  match term with
  | Variable v -> [v]
  | Predicate(_, terms) ->
    terms |> List.collect freeVariables
  | _ -> []

let rec withFreshVariables (clause:Clause) : Clause =
  // TODO: Get a list of distinct variables in the clause (using 
  // 'freeVariables' and 'List.distinct'), generate a substitution 
  // that append a number 'n' obtained by 'nextNumber()' to the end
  // of all the variable names, and apply the substitutions to the 
  // head and body of the clause.
  //
  // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
  // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
  //
  // This may not be correct if the user-provided names of variables
  // had numbers in them in a certain format, but that's OK for now! 
  let number = (nextNumber ()).ToString()
  let a = clause.Head |> freeVariables |> List.distinct |> List.collect (fun x -> [(x, Variable ([x; number] |> String.concat ""))])
  let b = clause.Body |> List.collect freeVariables |> List.distinct |> List.collect (fun x -> [(x, Variable ([x; number] |> String.concat ""))])
  let c = substitute (Map.ofList a) clause.Head
  let d = substituteTerms (Map.ofList b) clause.Body

  { 
    Head = c
    Body = d 
  }


let collectSubstitutions clause query =
  match (unify clause.Head query) with
  | Some(subst) -> [clause, subst]
  | None -> []


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  // TODO: Return all clauses from 'program' whose 'Head' can be
  // unified with the specified 'query' and return the resulting
  // substitutions. Before unifying, rename variables in the program
  // rule using 'withFreshVariables'. You can do this using 'List.choose' 
  // or by using list comprehension.
  // 
  // The return type of this is a list of tuples consisting of the matching
  // clause and a substitution (list<string * Term>). Calling 'unify'
  // gives you 'option<list<string * Term>>', so you need to pattern match
  // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.

  let renamedProgram = program |> List.collect (fun x -> [x |> withFreshVariables])
  renamedProgram |> List.collect (fun x -> collectSubstitutions x query)


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// // Query: male(X)
// // Match #1: male(William)
// // Match #2: male(Charles)
// // Match #3: male(George)
// query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
