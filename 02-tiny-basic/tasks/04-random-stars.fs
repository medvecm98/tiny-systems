// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { 
    Program : list<int * Command> 
    Context : Map<string, Value> 
    Random : System.Random
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue s ->
    printfn "%s" s
  | NumberValue i ->
    printfn "%d" i
  | BoolValue b ->
    printfn "%b" b

let rec getLineOfProgram(program: list<int * Command>, line: int) =
  match program with
    | (lineNumber, command)::tail ->
      if lineNumber = line then
        command
      else
        getLineOfProgram (tail, line)
    | [] ->
      failwith "No such line number found."

let rec getLine state line =
  (line, getLineOfProgram (state.Program, line))

let addLine state (line, cmd) = 
  {
    Program = state.Program |> List.filter (fun (l, c) -> l <> line) |> List.append [(line, cmd)] |> List.sortBy (fun (l, c) -> l)
    Context = state.Context
    Random = state.Random
  }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
  match expr with
  | Const c ->
    c
  | Function (_, []) ->
      failwith "Not enough arguments in funtion call"
  | Function (name, arg::[]) ->
    let value = evalExpression state arg
    match name with
    | "RND" ->
      match value with
      | NumberValue numVal ->
        NumberValue (state.Random.Next(numVal))
      | _ ->
        failwith "Trying to be random with non-numeric type."
    | _ ->
      failwith "Invalid function name and/or number of parameters."
  | Function (name, larg::rarg::_) ->
    let lval = evalExpression state larg
    let rval = evalExpression state rarg
    match name with
    | "-" ->
      match lval, rval with
      | NumberValue ln, NumberValue rn ->
        NumberValue(ln - rn)
      | _ ->
        failwith "Can't use strings or bools in subtraction."
    | "=" ->
      BoolValue(lval = rval)
    | "<" ->
      binaryRelOp (<) [lval; rval]
    | ">" ->
      binaryRelOp (>) [lval; rval]
    | "||" ->
      match lval, rval with
      | BoolValue ln, BoolValue rn ->
        BoolValue(ln || rn)
      | _ ->
        failwith "Can't use strings or numbers in binary OR."
    | _ ->
      failwith "Invalid function name and/or number of parameters."
  | Variable var ->
    let tryFind = Map.tryFind var state.Context
    match tryFind with
    | Some v ->
      v
    | None ->
      failwith "Invalid variable name"

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      (state, expr) ||> evalExpression |> printValue
      runNextLine state line
  | Goto(ln) ->
      getLine state ln |> runCommand state
  
  | Assign (name, expr) ->
    let value = evalExpression state expr
    let nctx = state.Context |> Map.add name value
    let nstate = {
      Program = state.Program
      Context = nctx
      Random = state.Random
    }
    runNextLine nstate line
  | If (expr, subCmd) ->
    match evalExpression state expr with
    | BoolValue false ->
      runNextLine state line
    | BoolValue true ->
      runCommand state (line, subCmd)
    | _ ->
      failwith "Invalid type in IF condition."
  
  // TODO: Implement two commands for screen manipulation
  | Clear -> 
    System.Console.Clear()
    runNextLine state line
  | Poke (x, y, ch) ->
    let valX = evalExpression state x
    let valY = evalExpression state y
    match valX, valY, ch with
    | NumberValue numX, NumberValue numY, Const(StringValue charVal) ->
      System.Console.CursorLeft <- numX
      System.Console.CursorTop <- numY
      System.Console.Write(charVal)
    | _ ->
      failwith "wertyuiop"
    runNextLine state line

and runNextLine state line = 
  match getLineOfProgramGreater(state.Program, line) with
    | None ->
      state
    | Some (-1, _) ->
      state
    | Some (l, c) ->
      runCommand state (l, c)

and getLineOfProgramGreater(program: list<int * Command>, line: int) =
  match program with
    | (lineNumber, command)::tail ->
      if lineNumber > line then
        Some (lineNumber, command)
      else
        getLineOfProgramGreater (tail, line)
    | [] ->
      None

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with
  | Some ln ->
    addLine state (ln, cmd)
  | None ->
    runCommand state (-1, cmd)

let runInputs state cmds =
  (state, cmds) ||> List.fold (fun s c -> runInput s c)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let rnd = System.Random()

let empty = { Program = []; Context = Map.empty; Random = System.Random() } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num System.Console.WindowWidth], "RND" @ [num System.Console.WindowHeight], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num System.Console.WindowWidth], "RND" @ [num System.Console.WindowHeight], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
