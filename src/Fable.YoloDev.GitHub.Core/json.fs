[<AutoOpen>]
module YoloDev.GitHubClient.Core.Json

open Fable.Core
open Aether

// Types
//
// Simple AST for JSON, with included isomorphisms and lenses in Aether form for
// lens/isomorphism based modification of complex JSON structures.

type Json =
| Null
| Bool   of bool
| Number of float
| String of string
| Array  of Json list
| Object of Map<string, Json>

module Json =
  // Epimorphisms
  let internal Null__ = (function | Null -> Some () | _ -> None), fun () -> Null
  let internal Bool__ = (function | Bool x -> Some x | _ -> None), Bool
  let internal Number__ = (function | Number x -> Some x | _ -> None), Number
  let internal String__ = (function | String x -> Some x | _ -> None), String
  let internal Array__ = (function | Array x -> Some x | _ -> None), Array
  let internal Object__ = (function | Object x -> Some x | _ -> None), Object

  // Prisms
  let Null_ = Prism.ofEpimorphism Null__
  let Bool_ = Prism.ofEpimorphism Bool__
  let Number_ = Prism.ofEpimorphism Number__
  let String_ = Prism.ofEpimorphism String__
  let Array_ = Prism.ofEpimorphism Array__
  let Object_ = Prism.ofEpimorphism Object__

// Functional
//
// Functional signatures for working with Json types, implying a monadic
// approach to working with Json where appropriate.
// 
// Additionally includes common functions for combining and creating
// functions of type Json<'a> which may be used via operator based
// combinators or a computation expression (both provided later).

[<AutoOpen>]
module Functional =
  type JsonResult<'a> =
  | Value of 'a
  | Error of string

  type Json<'a> = Json -> JsonResult<'a> * Json

  // Functions
  //
  // Common functions for combining Json<'a> functions in to new
  // forms, and for creating new Json<'a> functions given suitable
  // initial data.
  [<RequireQualifiedAccess>]
  module Json =
    let inline init (a: 'a) : Json<'a> =
      fun json -> Value a, json
    
    let inline error (e: string) : Json<'a> =
      fun json -> Error e, json
    
    let inline internal ofResult result =
      fun json -> result, json
    
    let inline bind (m: Json<'a>) (f: 'a -> Json<'b>) : Json<'b> =
      fun json ->
        match m json with
        | Value a, json -> (f a) json
        | Error e, json -> Error e, json
    
    let inline apply (f: Json<'a -> 'b>) (m: Json<'a>) : Json<'b> =
      bind f (fun f' ->
        bind m (fun m' ->
          init (f' m')))
    
    let inline map (f: 'a -> 'b) (m: Json<'a>) : Json<'b> =
      bind m (fun m' ->
        init (f m'))
    
    let inline map2 (f: 'a -> 'b -> 'c) (m1: Json<'a>) (m2: Json<'b>) : Json<'c> =
      apply (apply (init f) m1) m2

// Operators
//
// Symbolic operators for working with Json<'a> functions, providing
// an operator based concise alternative to the primitive Json<'a> combinators
// given as part of Functional.
//
// This module is not opened by default, as symbolic operators are a matter
// of taste and may also clash with other operators from other libraries.

module Operators =
  let inline (>>=) m f = Json.bind m f
  let inline (=<<) f m = Json.bind m f
  let inline (<*>) f m = Json.apply f m
  let inline (<!>) f m = Json.map f m
  let inline (>>.) m f = Json.bind m (fun _ -> f)
  let inline (.>>) m f = Json.bind (fun _ -> m) f
  let inline ( *>) m1 m2 = Json.map2 (fun _ x -> x) m1 m2
  let inline ( <*) m1 m2 = Json.map2 (fun x _ -> x) m1 m2
  let inline (>=>) m1 m2 = Json.bind (fun x -> m1 x) m2
  let inline (<=<) m1 m2 = Json.bind (fun x -> m2 x) m1

// Builder
//
// Computation expression (builder) for working with JSON structures in a
// simple way, including lensing, morphisms, etc. using the Aether
// library.

module Builder =
  type JsonBuilder () =
    member __.Bind (m1, m2) : Json<_> = Json.bind m1 m2
    member __.Combine (m1, m2) : Json<_> = Json.bind m1 (fun () -> m2)
    member __.Delay (f) : Json<_> = Json.bind (Json.init ()) f
    member __.Return (x) : Json<_> = Json.init x
    member __.ReturnFrom (f) : Json<_> = f
    member __.Zero () : Json<_> = Json.init ()

let json = Builder.JsonBuilder ()

// Optics
//
// Functional optics based access to nested Json data structures,
// using Aether format lenses/prisms/etc. Uses Json<'a> based functions, so
// can be used monadically.

[<AutoOpen>]
module Optics =
  // Functions
  [<RequireQualifiedAccess>]
  module Json =
    [<RequireQualifiedAccess>]
    module Optic =
      type Get =
        | Get with
          static member (^.) (Get, l: Lens<Json,'b>) : Json<_> =
            fun json -> Value (Optic.get l json), json
          
          static member (^.) (Get, p: Prism<Json,'b>) : Json<_> =
            fun json ->
              match Optic.get p json with
              | Some x -> Value x, json
              | _ -> Error (sprintf "Couldn't use Prism %A on JSON: '%A'" p json), json
      
      let inline get o : Json<_> = (Get ^. o)

      type TryGet =
        | TryGet with
          static member (^.) (TryGet, l: Lens<Json,'b>) : Json<_> =
            fun json -> Value (Some (Optic.get l json)), json
          
          static member (^.) (TryGet, p: Prism<Json,'b>) : Json<_> =
            fun json -> Value (Optic.get p json), json
      
      let inline tryGet o : Json<_> = (TryGet ^. o)
      let inline set o v : Json<_> = fun json -> Value (), Optic.set o v json
      let inline map o f : Json<_> = fun json -> Value (), Optic.map o f json


// Parsing
//
// Functions for converting POJOs to Json types.

[<AutoOpen>]
module Parsing =
  [<Emit("typeof $0")>]
  let private jsTypeof (o: obj): string = jsNative

  [<Emit("Array.isArray($0)")>]
  let private jsIsArray (o: obj): bool = jsNative

  [<Emit("Object.entries($0)")>]
  let private jsObjEntries (o: obj): (string * obj) array = jsNative

  [<Emit("$1[$0]")>]
  let private jsGetProp (prop: string, o: obj): 'a = jsNative

  [<Emit("JSON.parse($0)")>]
  let private jsJsonParse (json: string): obj = jsNative

  let private (|JsNull|JsBool|JsNumber|JsString|JsArray|JsObject|) (o: obj) =
    if   isNull o               then JsNull
    elif jsTypeof o = "boolean" then JsBool (o :?> bool)
    elif jsTypeof o = "number"  then JsNumber (o :?> float)
    elif jsTypeof o = "string"  then JsString (o :?> string)
    elif jsIsArray o            then JsArray (o :?> obj array)
    else                             JsObject (o |> jsObjEntries)

  let rec private convertValue (o: obj) =
    match o with
    | JsNull -> Json.Null
    | JsBool x -> Json.Bool x
    | JsNumber x -> Json.Number x
    | JsString x -> Json.String x
    | JsArray x -> x |> Seq.map convertValue |> List.ofSeq |> Json.Array
    | JsObject x -> x |> Seq.map (fun (k, v) -> k, convertValue v) |> Map.ofSeq |> Json.Object
  
  [<RequireQualifiedAccess>]
  module Json =
    let parse json = jsJsonParse json |> convertValue

// Formatting

[<AutoOpen>]
module Formatting =
  [<Emit("JSON.stringify($0)")>]
  let private jsJsonStringify (o: obj): string = jsNative
  [<Emit("JSON.stringify($0,null,$1)")>]
  let private jsJsonStringifyPretty (o: obj, indent: int): string = jsNative

  type JsonFormattingOptions = {
    spacing: int option
  }

  let private jsonStringifyWithFormat (fmt: JsonFormattingOptions) (obj: obj) =
    match fmt.spacing with
    | None -> jsJsonStringify obj
    | Some spacing -> jsJsonStringifyPretty (obj, spacing)

  [<RequireQualifiedAccess>]
  module JsonFormattingOptions =
    let compact = { spacing = None }
    let pretty = { spacing = Some 2 }

  let rec private convertValue =
    function
    | Null         -> null
    | Bool x       -> box x
    | Number x     -> box x
    | String x     -> box x
    | Array xs     -> xs |> Seq.map convertValue |> Array.ofSeq |> box
    | Object map   -> map |> Map.toSeq |> Seq.map (fun (k, v) -> k, convertValue v) |> Fable.Core.JsInterop.createObj
  
  [<RequireQualifiedAccess>]
  module Json =
    let format json = convertValue json |> jsonStringifyWithFormat JsonFormattingOptions.compact
    let formatWith format json = convertValue json |> jsonStringifyWithFormat format
  
  // Error Message Formatters
  [<RequireQualifiedAccess>]
  module Errors =
    let missingMember key =
      sprintf "Error deserializing JSON object; Missing required member '%s'" key
    
    let missingMemberWithJson key =
      function
      | Some format -> Json.formatWith format >> (+) (missingMember key + ": ")
      | None        -> fun _ -> missingMember key

// Mapping
// 
// Functional mapping between Json and native F# data structures,
// through statically inferred types. Types providing FromJson and
// ToJson static members with appropriate signatures can be
// seamlessly serialized and deserialized.
//
// This approach is the same as that taken by the Fleece library,
// credit for which is due to Mauricio Scheffer.

[<AutoOpen>]
module Mapping =
  open Operators

  // FromJson
  //
  // Default conversion functions (static members on FromJsonDefaults)
  // and statically inferred inline conversion functions for conversion
  // from Json to F# data structures.

  // Defaults
  type FromJsonDefaults = 
    | FromJsonDefaults with
      static member inline FromJson (_: unit) = Json.Optic.get Json.Null_
      static member inline FromJson (_: bool) = Json.Optic.get Json.Bool_
      static member inline FromJson (_: decimal) = id <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: float) = float <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: int) = int <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: int16) = int16 <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: int64) = int64 <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: single) = single <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: string) = Json.Optic.get Json.String_
      static member inline FromJson (_: uint16) = uint16 <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: uint32) = uint32 <!> Json.Optic.get Json.Number_
      static member inline FromJson (_: uint64) = uint64 <!> Json.Optic.get Json.Number_

      // Common Types
      static member inline FromJson (_: System.Guid) =
            fun x ->
                match System.Guid.TryParse x with
                | true, x -> Json.init x
                | _ -> Json.error "guid"
        =<< Json.Optic.get Json.String_
      
      // Json Type
      static member inline FromJson (_: Json) = Json.Optic.get id_

  // Mapping Functions
  //
  // Functions for applying the FromJson function to Json to produce
  // new instances of 'a where possible, including folding the FromJson
  // function across a list of Json objects.

  let inline internal fromJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b) : (static member FromJson: ^a -> ^a Json) a)
  
  let inline internal fromJson x =
    fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x)
  
  let inline internal fromJsonFold init fold xs =
    List.fold (fun r x ->
      match r with
      | Error e  -> Error e
      | Value xs ->
        match fromJson x with
        | Value x -> Value (fold x xs)
        | Error e -> Error e) (Value init) (List.rev xs)
  
  // Defaults

  type FromJsonDefaults with
    // Arrays
    static member inline FromJson (_: 'a array) : Json<'a array> =
        fromJsonFold [||] (fun x xs -> Array.append [| x |] xs) >> Json.ofResult
      =<< Json.Optic.get Json.Array_
    
    // Lists
    static member inline FromJson (_: 'a list) : Json<'a list> =
        fromJsonFold [] (fun x xs -> x :: xs) >> Json.ofResult
      =<< Json.Optic.get Json.Array_
    
    // Maps
    static member inline FromJson (_: Map<string,'a>) : Json<Map<string,'a>> =
        fun x ->
            let k, v = (Map.toList >> List.unzip) x
            List.zip k >> Map.ofList <!> Json.ofResult (fromJsonFold [] (fun x xs -> x :: xs) v)
      =<< Json.Optic.get Json.Object_
    
    // Sets
    static member inline FromJson (_: Set<'a>) : Json<Set<'a>> =
        fromJsonFold Set.empty Set.add >> Json.ofResult
      =<< Json.Optic.get Json.Array_
    
    // Options
    static member inline FromJson (_: 'a option) : Json<'a option> =
        function | Null _ -> Json.init None | x -> Some <!> Json.ofResult (fromJson x)
      =<< Json.Optic.get id_
    
    // Tuples
    static member inline FromJson (_: 'a * 'b) : Json<'a * 'b> =
        function | a :: [b] ->
                    fun a b -> a, b
                  <!> Json.ofResult (fromJson a)
                  <*> Json.ofResult (fromJson b)
                 | _ -> Json.error "tuple2"
      =<< Json.Optic.get Json.Array_
    
    static member inline FromJson (_: 'a * 'b * 'c) : Json<'a * 'b * 'c> =
        function | a :: b :: [c] ->
                    fun a b c -> a, b, c
                  <!> Json.ofResult (fromJson a)
                  <*> Json.ofResult (fromJson b)
                  <*> Json.ofResult (fromJson c)
                 | _ -> Json.error "tuple3"
      =<< Json.Optic.get Json.Array_
    
    static member inline FromJson (_: 'a * 'b * 'c * 'd) : Json<'a * 'b * 'c * 'd> =
        function | a :: b :: c :: [d] ->
                    fun a b c d -> a, b, c, d
                  <!> Json.ofResult (fromJson a)
                  <*> Json.ofResult (fromJson b)
                  <*> Json.ofResult (fromJson c)
                  <*> Json.ofResult (fromJson d)
                 | _ -> Json.error "tuple4"
      =<< Json.Optic.get Json.Array_
    
    static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e) : Json<'a * 'b * 'c * 'd * 'e> =
        function | a :: b :: c :: d :: [e] ->
                    fun a b c d e -> a, b, c, d, e
                  <!> Json.ofResult (fromJson a)
                  <*> Json.ofResult (fromJson b)
                  <*> Json.ofResult (fromJson c)
                  <*> Json.ofResult (fromJson d)
                  <*> Json.ofResult (fromJson e)
                 | _ -> Json.error "tuple2"
      =<< Json.Optic.get Json.Array_
  
  // ToJson

  // Defaults

  type ToJsonDefaults =
    | ToJsonDefaults with
      // Basic Types
      static member inline ToJson (x: unit) = Json.Optic.set Json.Null_ x
      static member inline ToJson (x: bool) = Json.Optic.set Json.Bool_ x
      static member inline ToJson (x: decimal) = Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: float) =
        match x with
        | x when System.Double.IsInfinity x -> failwith "Serialization of Infinite Numbers Invalid."
        | x when System.Double.IsNaN x      -> failwith "Serialization of NaN Invalid."
        | x                                 -> Json.Optic.set Json.Number_ x
      static member inline ToJson (x: int) = Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: int16) = Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: int64) = Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: single) =
        match x with
        | x when System.Single.IsInfinity x -> failwith "Serialization of Infinite Numbers Invalid."
        | x when System.Single.IsNaN x      -> failwith "Serialization of NaN Invalid."
        | x                                 -> Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: uint16) = Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: uint32) = Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: uint64) = Json.Optic.set Json.Number_ (float x)
      static member inline ToJson (x: string) = Json.Optic.set Json.String_ x

      // Common types
      static member inline ToJson (x: System.Guid) = Json.Optic.set Json.String_ (string x)

      // Json Type
      static member inline ToJson (x: Json) = Json.Optic.set id_ x
  
  // Mapping Functions
  //
  // Functions for applying the ToJson function to data structures to produce
  // new Json instances.

  let inline internal toJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b) : (static member ToJson: ^a -> unit Json) a)
  
  let inline internal toJson (x: 'a) =
    snd (toJsonDefaults (x, ToJsonDefaults) (Object (Map.empty)))
  
  let inline internal toJsonWith (f:'a -> unit Json) (x: 'a) = 
    snd (f x (Object (Map.empty))) 
  
  // Defaults
  //

  type ToJsonDefaults with
    // Arrays
    static member inline ToJson (x: 'a array) =
      Json.Optic.set id_ (Array ((Array.toList >> List.map toJson) x))
    
    // Lists
    static member inline ToJson (x: 'a list) =
      Json.Optic.set id_ (Array (List.map toJson x))
    
    // Maps
    static member inline ToJson (x: Map<string,'a>) =
      Json.Optic.set id_ (Object (Map.map (fun _ a -> toJson a) x))
    
    // Options
    static member inline ToJson (x: 'a option) =
      Json.Optic.set id_ ((function | Some a -> toJson a | _ -> Null) x)
    
    // Sets
    static member inline ToJson (x: Set<'a>) =
      Json.Optic.set id_ (Array ((Set.toList >> List.map toJson) x))
    
    // Tuples
    static member inline ToJson ((a, b)) =
      Json.Optic.set id_ (Array [ toJson a; toJson b ])
    
    static member inline ToJson ((a, b, c)) =
      Json.Optic.set id_ (Array [ toJson a; toJson b; toJson c ])
    
    static member inline ToJson ((a, b, c, d)) =
      Json.Optic.set id_ (Array [ toJson a; toJson b; toJson c; toJson d ])
    
    static member inline ToJson ((a, b, c, d, e)) =
      Json.Optic.set id_ (Array [ toJson a; toJson b; toJson c; toJson d; toJson e ])
  
  // Functions
  //

  [<RequireQualifiedAccess>]
  module Json =
    // Read/Write

    let missingMember key =
      fun json ->
        Errors.missingMemberWithJson key (Some JsonFormattingOptions.compact) json
        |> fun e -> Error e, json
    
    let readMemberWith fromJson key onMissing =
        Json.Optic.tryGet (Json.Object_ >?> Map.key_ key)
      >>= function | Some json -> Json.ofResult (fromJson json)
                   | None -> onMissing ()
    
    let inline readWith fromJson key =
      readMemberWith fromJson key <| fun () -> missingMember key
    
    let inline read key =
      readWith fromJson key
    
    let inline readWithOrDefault fromJson key def =
      readMemberWith fromJson key <| fun () -> Json.init def
    
    let inline readOrDefault key def =
      readWithOrDefault fromJson key def
    
    let inline tryReadWith fromJson key =
      readMemberWith fromJson key <| fun () -> Json.init None
    
    let inline tryRead key =
      tryReadWith fromJson key
    
    let writeWith toJson key value =
      Json.Optic.set (Json.Object_ >?> Map.value_ key) (Some (toJson value))
    
    let inline write key value =
      writeWith toJson key value
    
    let writeWithUnlessDefault toJson key def value =
      match value with
      | v when v = def -> Json.ofResult <| Value ()
      | _ -> writeWith toJson key value
    
    let inline writeUnlessDefault key def value =
      writeWithUnlessDefault toJson key def value
    
    let writeNone key = Json.Optic.set (Json.Object_ >?> Map.value_ key) (Some Json.Null)

    // Serialization/Deserialization

    let inline deserialize json =
      fromJson json
      |> function | Value a -> a
                  | Error e -> failwith e
    
    let inline tryDeserialize json =
      fromJson json
      |> function | Value a -> Choice1Of2 a
                  | Error e -> Choice2Of2 e
    
    let inline serialize a = toJson a
    let inline serializeWith f a = toJsonWith f a

// Patterns
//
// Active patterns for working with Json data structures, making it
// easier to write code for matching against unions, etc.

[<AutoOpen>]
module Patterns =
  open Aether.Operators

  /// Parse a Property from a Json Object token using a supplied fromJson,
  /// and try to deserialize it to the inferred type.
  let inline (|PropertyWith|) fromJson key =
      Optic.get (Json.Object_ >?> Map.key_ key)
    >> Option.bind (fromJson >> function | Value a, _ -> Some a
                                         | _ -> None)
  
  /// Parse a Property from a Json Object token, and try to deserialize it to the
  /// inferred type.
  let inline (|Property|_|) key =
      Optic.get (Json.Object_ >?> Map.key_ key)
    >> Option.bind (Json.tryDeserialize >> function | Choice1Of2 a -> Some a
                                                    | _ -> None)