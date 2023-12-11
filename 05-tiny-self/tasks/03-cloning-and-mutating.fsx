// ----------------------------------------------------------------------------
// 03 - Cloning and mutating TinySelf objects
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

// TODO: To implement assignment, we need to know what object a slot
// comes from. Modify 'lookup' so that it returns not just the slot,
// but also the object that the slot comes from.
let rec lookup msg obj : list<Objekt * Slot> = 
  match obj.Slots |> List.tryFind (fun s -> s.Name = msg) with
  | Some(slot) ->
    [obj, slot]
  | None ->
    obj.Slots |> List.collect (fun s -> lookup msg s.Contents)

// TODO: Modify 'send' and 'eval' to also take message send arguments.
// In Self, the arguments are copied into the activation record. 
// In TinySelf, we use simpler trick - just make the 'args' object 
// another parent of the activation record! Lookup for argument name 
// in the activation record will then give us the value.
// NOTE: The object newly returned from 'lookup' should be ignored.
// BEWARE: All arguments are 'Objekt' so it is easy to swap them!! 
let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue.Code with
  | None -> slotValue
  | Some(code) ->
    match code.Special with
    | None -> failwith "no special object"
    | Some(specUnion) -> 
      match specUnion with
      | String(s) -> failwith "special object non-native"
      | Native(func) ->
        let cloned = cloneObject(slotValue)
        addParentSlot "self*" instance cloned
        addParentSlot "args*" args cloned
        func(cloned)

let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
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
  // TODO: The activation record contains a slot 'self*' which is the
  // target object. Use lookup to get it, clone it & retrn it!
  // (If the lookup returns a wrong thing, fail - that's wrong.)
    let lookups = lookup "self*" arcd
    if List.length lookups = 1 then
      match lookups.[0] with
      | self, slot ->
        cloneObject slot.Contents
    else
      failwith "makeNativeMethod lookup fail"
  )

let clonablePrototype = 
  // TODO: Create an object that has the 'clone' method
  {Slots = [{Name = "clone"; Contents = cloneMethod; IsParent = false}]; Code = None; Special = None}

let assignmentMethod n = makeNativeMethod (fun arcd -> 
  // TODO: The activation record has a slot named 'n' somewhere in its
  // inheritance graph and a slot 'new' which is a method argument.
  // Find those two using 'lookup' and modify the slot value (in the 
  // that contained it - as returned from lookup). (Tiny)Self assignment 
  // should return the object that has been modified.
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
// Tests - cloning and modifying cats
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let mogscats = makeDataObject [
  makeSlot "book" (makeString "Mog's Family of Cats")
  // NOTE: This allows us to rename the book (probably not
  // something you'd want to do, but for illustration purposes...)
  makeAssignmentSlot "book"
]
let mog = makeDataObject [
  // NOTE: Mog is now also clonable and has assignment slot "name:"
  makeParentSlot "parent*" cat
  makeParentSlot "clonable*" clonablePrototype
  makeParentSlot "fictional*" mogscats
  makeSlot "name" (makeString "Mog")
  makeAssignmentSlot "name"
]

// NOTE: We now pass empty arguments to all of the message sends
mog |> send "name" empty |> send "print" empty
mog |> send "sound" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty

// NOTE: Clone Ginger and print its name & book
let ginger = mog |> send "clone" empty

// ginger |> send "name" empty |> send "print" empty
// ginger |> send "book" empty |> send "print" empty

// TODO: Write code to change the name of 'ginger' to "Ginger"!
// (send message "name:" with arument containing slot 'new' with the new value)
ginger 
|> send "name:" {Slots=[makeSlot "new" (makeString "Ginger")]; Code = None; Special = None}

// // TODO: Write code to change the book of 'ginger' to "Goodbye, Mog"!
ginger 
|> send "book:" {Slots=[makeSlot "new" (makeString "Goodbye, Mog")]; Code = None; Special = None}

// TODO: What do we get if we run the following now?
mog |> send "name" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty
ginger |> send "name" empty |> send "print" empty
ginger |> send "book" empty |> send "print" empty
