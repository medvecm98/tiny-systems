// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { 
    Program : list<int * Command>
    Context : Map<string, Value>
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
  }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr = 
  // TODO: Add support for 'Function' and 'Variable'. For now, handle just the two
  // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
  // (takes two values and returns Boolean). Note that you can test if two
  // F# values are the same using '='. It works on values of type 'Value' too.
  //
  // HINT: You will need to pass the program state to 'evalExpression' 
  // in order to be able to handle variables!
  match expr with
  | Const c ->
    c
  | Function (_, _::[]) | Function (_, []) ->
      failwith "Not enough arguments in funtion call"
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
    | _ ->
      failwith "Invalid function or binary operator."
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

let empty = { 
  Program = []
  Context = Map.empty
} // TODO: Add empty variables to the initial state!

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloNew = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD 10\n")) 
    Some 20, Print (Const (StringValue "HELLO NPRG077 20\n")) 
    Some 30, Print (Const (StringValue "HELLO NPRG077 30\n")) 
    Some 40, Print (Const (StringValue "HELLO NPRG077 40\n")) 
    Some 50, Print (Const (StringValue "HELLO NPRG077 50\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
