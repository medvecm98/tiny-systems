// ----------------------------------------------------------------------------
// 04 - Representing & interpreting TinySelf expressions
// ----------------------------------------------------------------------------

type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
let makeDataObject slots = 
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject slots special = 
  { Code = None; Special = Some special; Slots = slots }

let makeSlot n contents = 
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot n contents = 
  { Name = n; Contents = contents; IsParent = true }

let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

// NOTE: Implemented in step #2
let addSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  obj.Slots <- ([obj.Slots; [{Name = n; Contents = contents; IsParent = false}]] |> List.concat)

let addParentSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  obj.Slots <- ([obj.Slots; [{Name = n; Contents = contents; IsParent = true}]] |> List.concat)

let cloneObject (obj:Objekt) : Objekt = 
  {Slots = obj.Slots; Code = obj.Code; Special = obj.Special}

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup msg obj : list<Objekt * Slot> = 
  match obj.Slots |> List.tryFind (fun s -> s.Name = msg) with
  | Some(slot) ->
    [obj, slot]
  | None ->
    obj.Slots |> List.collect (fun s -> lookup msg s.Contents)

// TODO: If 'slotValue' has some non-native 'Code', we want to evaluate it.
// But it will be easier to add this later, so copy code from step 3 and then
// return to it later (there is a TODO below telling you to do this).
//
// If the slot contains 'Some(code)', we run it by sending the 'eval' message
// to the 'code'. The method takes the activation record (the same as in the
// case of native code) as the argument, so create data object with 'activation' 
// slot and pass the as argument when calling 'eval'.
// (to call 'send', you will need to use 'let rec .. and ..' here)
let rec eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue.Code with
  | None -> slotValue
  | Some(code) ->
    match code.Special with
    | None ->
      let dataObj = makeDataObject [makeSlot "activation" ]
      send "eval" dataObj code
    | Some(specUnion) -> 
      match specUnion with
      | String(s) -> failwith "special object non-native"
      | Native(func) ->
        let cloned = cloneObject(slotValue)
        addParentSlot "self*" instance cloned
        addParentSlot "args*" args cloned
        func(cloned)

and send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
  let slots = lookup msg instance
  if List.length slots <> 1 then
    failwith (sprintf "More than one receiver %s" msg)
  else
    match slots.[0] with
    | _, slot -> 
      eval slot.Contents args instance

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  // NOTE: We ignore the object returned by 'lookup' here.
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// NOTE: Implemented in step #2
let empty : Objekt = {Slots = []; Code = None; Special = None}

let printCode = makeNativeMethod (fun arcd ->
  let spec = send "value" empty arcd
  match spec.Special with
  | Some(String(s)) ->
    printfn "%s" s
  | _ -> failwith "what"
  empty
)
let stringPrototype = makeDataObject [
  makeSlot "print" printCode  
]
let makeString s = 
  makeDataObject [ 
    makeSlot "value" (makeSpecialObject [] (String s)) 
    makeParentSlot "parent*" stringPrototype
  ]

// ----------------------------------------------------------------------------
// Cloning and assignments
// ----------------------------------------------------------------------------

let cloneMethod = makeNativeMethod (fun arcd -> 
    let lookups = lookup "self*" arcd
    if List.length lookups = 1 then
      match lookups.[0] with
      | self, slot ->
        cloneObject slot.Contents
    else
      failwith "makeNativeMethod lookup fail"
  )

let clonablePrototype = 
  {Slots = [{Name = "clone"; Contents = cloneMethod; IsParent = false}]; Code = None; Special = None}

let assignmentMethod n = makeNativeMethod (fun arcd -> 
    let nLookup = lookup n arcd
    let newLookup = lookup "new" arcd
    match nLookup.[0] with
    | obj1, slot1 ->
      match newLookup.[0] with
      | obj2, slot2 ->
        obj1.Slots <- (obj1.Slots |> List.map (fun s -> if s.Name = n then {Name = n; Contents = slot2.Contents; IsParent = slot2.IsParent} else s))
        obj1
  )

// Creates an assignment slot for a slot named 'n'
let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }

// ----------------------------------------------------------------------------
// TinySelf code representation & interpreter
// ----------------------------------------------------------------------------

// NOTE: All code is represented as objects with 'eval' method. 
// The 'eval' method receives argument 'evalTarget' which represents 
// the object of the method containing the code that is being invoked.
// The 'eval' method is a native method implemented as F# function. We
// store the parameters of the expressions in the objects themselves 
// (and not in F# closures) to keep more things in the TinySelf world.


// TODO: 'self' expression has no arguments and needs no state. When 
// evaluated, it needs to get the original 'activation' we constructed
// when calling 'eval' - this is done by looking up 'activation' slot
// in the activation record with which the method is called - and then 
// from that, we can get the 'self*' slot.
// 
// NOTE: This is a bit confusing - the F# function we write gets 'arcd'
// which is the activation record for the 'eval' call. But this in turn 
// contains activation record 'activation' which is the activation record
// for the message send that our interpreter is handling!
let exprSelf = makeDataObject [
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    let obj = lookupSlotValue "activation" arcd
    lookupSlotValue "self*" obj
  ))
] 
  
// DEMO: 'string' expression stores the string value in a slot 'str'. When 
// evaluated, it fetches 'str' (slot value) from the activation record.
let exprString (s:string) = makeDataObject [ 
  makeSlot "string" (makeString s) 
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    lookupSlotValue "string" arcd
  )) ]

let exprSend msg rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun arcd -> 
    // TODO: To evalaute 'send' expression, we need to:
    // * Get 'activation' (activation record of the method call we are 
    ///  interpreting) from the activation record 'arcd' and create
    //   a new data object with this as the 'activation' to be used
    //   as an argument of recursive 'eval' call(s) later
    // * Get the string value of 'msg' slot (lookup from the 'acrd')
    // * Get the receiver expression (from the 'acrd')
    //   and evaluate it by send it 'eval' with the data object 
    //   (containing 'activation') as argument
    // * Send 'msg' to the recursively evaluated receiver object!
    failwith "not implemented"
  )) ]

// ----------------------------------------------------------------------------
// Tests - hello world (finally!)
// ----------------------------------------------------------------------------

// TinySelf code that sends "print" to a Hello world string
let helloCode =
  exprString "Hello world!!" |> exprSend "print" 

// Run it! We need to create arguments for 'eval' with 'activation'
// but since we are not using it (no exprSelf), it can be empty.
let emptySelf = makeDataObject [makeSlot "activation" empty]
helloCode |> send "eval" emptySelf |> ignore

// TODO: Now go back to the missing case in 'eval'. 
// We now add code as methods to a TinySelf object.



// Object with 'hello' method that prints hello world!
let helloObj = makeDataObject [ 
  makeSlot "hello" (makeObject [] helloCode) 
]
helloObj |> send "hello" empty |> ignore

// TODO: A more interesting example! Create an object with 
// string slot 'sound' (Meow! if you like cats) and a 'speak'
// method that sends 'sound' to self (use exprSelf) and then
// sends 'print' to the result to print it.
let animalObj = failwith "not implemented"

animalObj |> send "speak" empty |> ignore
