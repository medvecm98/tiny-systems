// ----------------------------------------------------------------------------
// 02 - Implementing (basic) message sending
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


// Native method has a special object (F# function) as code
let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

let addSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  // TODO: Make a new non-parent slot and add it to 'Slots' 
  // (create a new list and assign it to the 'Slots' field.)
  obj.Slots <- ([obj.Slots; [{Name = n; Contents = contents; IsParent = false}]] |> List.concat)

let addParentSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  // TODO: Make a new parent slot and add it to 'Slots' 
  // (create a new list and assign it to the 'Slots' field.)
  obj.Slots <- ([obj.Slots; [{Name = n; Contents = contents; IsParent = true}]] |> List.concat)

let cloneObject (obj:Objekt) : Objekt = 
  // TODO: Return a new 'Objekt' with exactly the same slots, code & special
  // as 'obj' (we are not doing "deep copy" - the two clones will share
  // references to the same objects in after their slots are copied)
  {Slots = obj.Slots; Code = obj.Code; Special = obj.Special}

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup (msg:string) (obj:Objekt) : list<Slot> = 
  match obj.Slots |> List.tryFind (fun s -> s.Name = msg) with
  | Some(slot) ->
    [slot]
  | None ->
    obj.Slots |> List.collect (fun s -> lookup msg s.Contents)

// See also �3.3.7 (https://handbook.selflanguage.org/SelfHandbook2017.1.pdf)
//
// Note that we do not need special "primitive sends". Instead, we have special
// objects and so we need to run the "native" method when we it is called.
//
// Also not that we do not yet support passing arguments to methods!

let eval (slotValue:Objekt) (instance:Objekt) =
  // TODO: Implement the evaluation logic:
  // * If the 'slotValue' is a data object (has no 'Code') it is returned
  // * If the 'slotValue' has 'Code', we should invoke it. For now, we only
  //   handle the case where 'Code' is 'Special' and has 'Native' method.
  // * If the 'slotValue' has 'Code' that'ss not 'Special' fail (for now)
  //
  // To run the method we need to clone the method object (using cloneObject),
  // make the receiver as the parent of the clone (using addParentSlot)
  // and invoke the native function with the clone as argument.
  //
  // NOTE: Why do we set the receiver as parent of the activation record?
  // We can then send messages to it directly to access the receiver's slots!
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
        func(cloned)



let send (msg:string) (instance:Objekt) : Objekt =
  // TODO: Use 'lookup' to find slots with the name of the message 'msg'. If
  // there is exactly one, evaluate it using 'eval', otherwise report an error.
  let slots = lookup msg instance
  if List.length slots <> 1 then
    failwith "More than one receiver"
  else
    eval slots.[0].Contents instance


// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  | [ { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// TODO: Define empty object with no data in it (needed below)
let empty : Objekt = {Slots = []; Code = None; Special = None}

let printCode = makeNativeMethod (fun arcd ->
  // TODO: Print the string value! To get the string, you can send 'value' 
  // to the activation record (because this has the receiver string as a 
  // parent). The returned object will be 'Special' with 'String' in it.
  // The function needs to return 'Objekt' - you can return 'empty'.
  let spec = send "value" arcd
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
    // TODO: Make 'stringPrototype' a parent of this string 
    // object so that we can send the 'print' message to it!
    makeParentSlot "parent*" stringPrototype
  ]

// ----------------------------------------------------------------------------
// Tests - still lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let wonderland = makeDataObject [
  makeSlot "book" (makeString "Alice in Wonderland")
]

let larry = makeDataObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
]

// // Sending 'book' to 'larry' will now throw 'message not understood'!
// larry |> send "name" |> send "print"
// larry |> send "sound" |> send "print"
// larry |> send "book" |> send "print"

let cheshire = makeDataObject [
  makeParentSlot "parent*" cat
  makeParentSlot "characterIn*" wonderland
  makeSlot "name" (makeString "Cheshire Cat")
]

// All of these should be OK!
cheshire |> send "name" |> send "print" 
cheshire |> send "sound" |> send "print"
cheshire |> send "book" |> send "print"
