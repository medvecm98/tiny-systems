// ----------------------------------------------------------------------------
// 05 - Add a simple data type - tuples
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following from before
  | Unary(op, e) ->
      // TODO: Implement the case for 'Unary' here!
      let v = evaluate ctx e
      match v with
      | ValNum n1 ->
        match op with
        | "-" -> ValNum(-1 * n1)
        | "+" -> ValNum(n1)
        | _ -> failwith "Invalid unary operator"
      | _ ->
        failwith "Invalid usage"
  | If(cond, e2, e3) ->
      let cv = evaluate ctx cond
      match cv with
      | ValNum n ->
        match n with
        | 1 -> evaluate ctx e2
        | _ -> evaluate ctx e3
      | _ ->
        failwith "Ivalid usage"
  
  | Lambda(v:string, e:Expression) ->
      // TODO: Evaluate a lambda - create a closure value
      ValClosure(v, e, ctx)

  | Application(e1, e2) ->
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
      let val2 = evaluate ctx e2
      let val1 = evaluate ctx e1
      match val2, val1 with
      | ValNum n, ValClosure (name, expr, ctx) ->
        evaluate (Map.add name val2 ctx) expr
      | _, _ ->
        failwith "AAAAAA"

  | Let(v, e1, e2) ->
    // TODO: There are two ways to do this! A nice tricky is to 
    // treat 'let' as a syntactic sugar and transform it to the
    // 'desugared' expression and evaluating that :-)
    match e1 with
    | Lambda(name, expr) ->
      let nctx:VariableContext = Map.add v (evaluate ctx e1) ctx
      evaluate nctx e2
    | _ ->
      let vl = evaluate ctx e1
      match vl with
      | ValNum n ->
        let nctx:VariableContext = Map.add v (ValNum(n)) ctx
        evaluate nctx e2
      | _ ->
        failwith("yolo2")

  | Tuple(e1, e2) ->
      ValTuple(evaluate ctx e1, evaluate ctx e2)

  | TupleGet(b, e) ->
    match e with
    | Tuple _->
      let valTuple = evaluate ctx e
      match valTuple with
      | ValTuple(v1, v2) ->
        if b then
          v1
        else
          v2
      | _ ->
        failwith("err1234567")
    | _ ->
      failwith("wertyui")

      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1= 
  TupleGet(true, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed1

let ed2 = 
  TupleGet(false, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed2

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = 
  TupleGet(true, Constant(42))
evaluate Map.empty ed3
