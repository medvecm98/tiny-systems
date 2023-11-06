// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue s ->
    printfn "%s" s

let rec getLineOfProgram(program: list<int * Command>, line: int) =
  match program with
    | (lineNumber, command)::tail ->
      if lineNumber = line then
        command
      else
        getLineOfProgram (tail, line)
    | [] ->
      failwith "No such line number found."

let rec getLine (state: State, line: int) =
  getLineOfProgram (state.Program, line)

let addLine state (line, cmd) = 
  // TODO: Add a given line to the program state. This should overwrite 
  // a previous line (if there is one with the same number) and also ensure
  // that state.Program is sorted by the line number.
  // HINT: Use List.filter and List.sortBy. Use F# Interactive to test them!
  { Program = state.Program |> List.filter (fun (l, c) -> l <> line) |> List.append [(line, cmd)] |> List.sortBy (fun (l, c) -> l) }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr = 
  match expr with
  | Const c ->
    c

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      expr |> evalExpression |> printValue
      runNextLine state line
  | Goto(ln) ->
      runCommand state (ln, getLine(state, ln))

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
        Some (line, command)
      else
        getLineOfProgramGreater (tail, line)
    | [] ->
      None

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  // TODO: Simulate what happens when the user enters a line of code in the 
  // interactive terminal. If the 'line' number is 'Some ln', we want to 
  // insert the line into the right location of the program (addLine); if it
  // is 'None', then we want to run it immediately. To make sure that 
  // 'runCommand' does not try to run anything afterwards, you can pass 
  // 'System.Int32.MaxValue' as the line number to it (or you could use -1
  // and handle that case specially in 'runNextLine')
  match line with
  | Some ln ->
    addLine state (ln, cmd)
  | None ->
    runCommand state (-1, cmd)
      

let runInputs state cmds =
  // TODO: Apply all the specified commands to the program state using 'runInput'.
  // This is a one-liner if you use 'List.fold' which has the following type:
  //   ('State -> 'T -> 'State) -> 'State -> list<'T>
  (state, cmds) ||> List.fold (fun s c -> runInput s c)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
