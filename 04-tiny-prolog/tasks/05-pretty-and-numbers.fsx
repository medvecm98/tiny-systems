// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with
  | Atom("zero") -> Some(0)
  | Predicate("succ", [n]) ->
    match n with
    | Number n -> Some (n + 1)
    | _ -> None
  | _ -> 
    // TODO: Write an active pattern to recognize numbers in the form used below.
    // If the term is 'Atom("zero")' return Some(0). 
    // If the term is 'Predicate("succ", [n])' where 'n' is itself
    // a term representing number, return the number value +1. 
    None


let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      // TODO: format all arguments recursively using 'formatTerm'
      // You can then concatenate the arguments using 'String.concat'
      String.concat "" (p::": "::(List.map formatTerm items))
      // (String.concat "" [p; ": "], items) ||> List.fold (fun acc term -> String.concat " " [acc; (formatTerm term)])

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with
  | Variable v -> [v]
  | Predicate(_, terms) ->
    terms |> List.collect freeVariables
  | _ -> []

let rec withFreshVariables (clause:Clause) : Clause =
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
  let renamedProgram = program |> List.collect (fun x -> [x |> withFreshVariables])
  renamedProgram |> List.collect (fun x -> collectSubstitutions x query)

let rec solve program subst goals =
  // TODO: When printing the computed substitution 'subst', print
  // the terms nicely using 'formatTerm'. You can use 'for' loop like:
  // 'for var, term in subst do printfn ...'
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = List.concat [goals; clause.Body]
        let substNewGoals = substituteTerms (Map.ofList newSubst) newGoals
        let newNewSubst = substituteSubst (Map.ofList newSubst) subst
        let substAppend = List.concat [newNewSubst; newSubst]
        solve program substAppend substNewGoals

  | [] -> 
    printfn "\n== OUTPUT BEGIN ==\n"
    for var, term in subst do
      printf " %s = " var
      printf "%s," (formatTerm term)
    printfn ","
    printfn "\n== OUTPUT  END  ==\n"

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// // Queries from previous step (now with readable output)
// solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
// solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  match n with
  | 0 -> Atom("zero")
  | m -> Predicate("succ", [num (m - 1)])


// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// // Query: add(2, 3, X)
// // Output should include: 'X = 5' 
// //   (and other variables resulting from recursive calls)
// solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// // Query: add(2, X, 5)
// // Output should include: 'X = 3' 
// //   (we can use 'add' to calculate subtraction too!)
// solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
