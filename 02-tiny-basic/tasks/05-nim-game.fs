// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

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
    printf "%s" s
  | NumberValue i ->
    printf "%d" i
  | BoolValue b ->
    printf "%b" b

let rec printValues values =
  match values with
  | value::[] ->
    printValue value
  | value::tail ->
    printValue value
    printValues tail
  | _ ->
    printfn "Empty list of values to print."

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

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpressions state exprList =
  match exprList with
    | expr::[] ->
      let value = evalExpression state expr
      [value]
    | expr::tail ->
      let value = evalExpression state expr
      let values = evalExpressions state tail
      List.append [value] values
    | _ ->
      failwith "Empty list of expressions to evaluate."

and evalExpression state expr = 
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
    | "MIN" ->
      match lval, rval with
      | NumberValue ln, NumberValue rn ->
        NumberValue(System.Math.Min(ln, rn))
      | _ ->
        failwith "Can't use strings or bools in MIN function."
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
    (state, expr) ||> evalExpressions |> printValues
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

  // TODO: Input("X") should read a number from the console using Console.RadLine
  // and parse it as a number using Int32.TryParse (retry if the input is wrong)
  // Stop terminates the execution (you can just return the 'state'.)
  | Input varName ->
    match System.Int32.TryParse(System.Console.ReadLine()) with
    | true, num ->
      let numVal = NumberValue(num)
      let nctx = state.Context |> Map.add varName numVal
      let nstate = {
        Program = state.Program
        Context = nctx
        Random = state.Random
      }
      runNextLine nstate line
    | _ ->
      printfn "Parsing number failed. Enter valid Int32 number please."
      runCommand state (line, cmd)

  | Stop ->
    state

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Context = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
