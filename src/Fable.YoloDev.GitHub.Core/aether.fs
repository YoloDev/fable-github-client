module YoloDev.GitHubClient.Core.Aether

open Fable.Core

//  Optics
/// Lens from 'a -> 'b.
type Lens<'a, 'b> = ('a -> 'b) * ('b -> 'a -> 'a)

/// Prism from 'a -> 'b.
type Prism<'a, 'b> = ('a -> 'b option) * ('b -> 'a -> 'a)

//  Morphisms
/// Isomorphism between 'a <> 'b.
type Isomorphism<'a, 'b> = ('a -> 'b) * ('b -> 'a)

/// Epimorphism between 'a <> 'b.
type Epimorphism<'a, 'b> = ('a -> 'b option) * ('b -> 'a)

/// Functions for composing lenses and prisms with other optics, which
/// returns a new lens or prism based on the optic composed. Open `Aether.Operators`
/// to use the infix operator forms of these compositions, which is significantly
/// less verbose.
[<RequireQualifiedAccess>]
module Compose =
  /// Static overloads of the composition function for lenses (>->).
  /// These functions do not generally need to be called directly, but will
  /// be used when calling Compose.optic.
  type Lens =
    | Lens with
      static member (>->) (Lens, (g2, s2): Lens<'b, 'c>) =
        fun ((g1, s1): Lens<'a, 'b>) ->
          (fun a -> g2 (g1 a)),
          (fun c a -> s1 (s2 c (g1 a)) a) : Lens<'a, 'c>
      
      static member (>->) (Lens, (g2, s2): Prism<'b, 'c>) =
        fun ((g1, s1): Lens<'a, 'b>) ->
          (fun a -> g2 (g1 a)),
          (fun c a -> s1 (s2 c (g1 a)) a) : Prism<'a, 'c>
      
      static member (>->) (Lens, (f, t): Isomorphism<'b, 'c>) =
        fun ((g, s): Lens<'a, 'b>) ->
          (fun a -> f (g a)),
          (fun c a -> s (t c) a) : Lens<'a, 'c>
      
      static member (>->) (Lens, (f, t): Epimorphism<'b,'c>) =
        fun ((g, s): Lens<'a, 'b>) ->
          (fun a -> f (g a)),
          (fun c a -> s (t c) a) : Prism<'a, 'c>
    
  /// Compose a lens with an optic or morphism.
  let inline lens l o = (Lens >-> o) l

  /// Static overloads of the composition function for prisms (>?>).
  /// These functions do not generally need to be called directly, but will
  /// be used when calling Compose.optic.
  type Prism =
    | Prism with
      static member (>?>) (Prism, (g2, s2): Lens<'b,'c>) =
        fun ((g1, s1): Prism<'a,'b>) ->
          (fun a -> Option.map g2 (g1 a)),
          (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                           | _ -> a) : Prism<'a,'c>
      
      static member (>?>) (Prism, (g2, s2): Prism<'b,'c>) =
        fun ((g1, s1): Prism<'a,'b>) ->
          (fun a -> Option.bind g2 (g1 a)),
          (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                           | _ -> a) : Prism<'a,'c>

      static member (>?>) (Prism, (f, t): Isomorphism<'b,'c>) =
        fun ((g, s): Prism<'a,'b>) ->
          (fun a -> Option.map f (g a)),
          (fun c a -> s (t c) a) : Prism<'a,'c>
      
      static member (>?>) (Prism, (f, t): Epimorphism<'b,'c>) =
        fun ((g, s): Prism<'a,'b>) ->
          (fun a -> Option.bind f (g a)),
          (fun c a -> s (t c) a) : Prism<'a,'c>
  
  /// Compose a prism with an optic or morphism.
  let inline prism p o = (Prism >?> o) p

/// Functions for using optics to operate on data structures, using the basic optic
/// operations of get, set and map. The functions are overloaded to take either lenses or
/// prisms, with the return type being inferred.
[<RequireQualifiedAccess>]
module Optic =
  /// Static overloads of the optic get function (^.). These functions do not generally
  /// need to be called directly, but will be used when calling Optic.get.
  type Get =
    | Get with
      static member (^.) (Get, (g, _): Lens<'a,'b>) =
        fun (a: 'a) -> g a : 'b
      
      static member (^.) (Get, (g, _): Prism<'a,'b>) =
        fun (a: 'a) -> g a : 'b option
  
  /// Get a value using an optic.
  let inline get optic target = (Get ^. optic) target

  /// Static overloads of the optic set function (^=). These functions do
  /// not generally need to be called directly, but will be used when calling
  /// Optic.set.
  type Set =
    | Set with
      static member (^=) (Set, (_, s): Lens<'a,'b>) =
        fun (b: 'b) -> s b : 'a -> 'a
      
      static member (^=) (Set, (_, s): Prism<'a,'b>) =
        fun (b: 'b) -> s b : 'a -> 'a
  
  /// Set a value using an optic.
  let inline set optic value = (Set ^= optic) value

  /// Static overloads of the optic map function (%=). These functions do not generally
  /// need to be called directly, but will be used when calling Optic.map.
  type Map =
    | Map with
      static member (^%) (Map, (g, s): Lens<'a,'b>) =
        fun (f: 'b -> 'b) -> (fun a -> s (f (g a)) a) : 'a -> 'a
      
      static member (^%) (Map, (g, s): Prism<'a,'b>) =
        fun (f: 'b -> 'b) -> (fun a -> Option.map f (g a) |> function | Some b -> s b a
                                                                      | _ -> a) : 'a -> 'a

  /// Modify a value using an optic.
  let inline map optic f = (Map ^% optic) f

/// Functions for creating or using lenses.
[<RequireQualifiedAccess>]
module Lens =
  /// Converts an isomorphism into a lens.
  let ofIsomorphism ((f, t): Isomorphism<'a,'b>) : Lens<'a,'b> = f, (fun b _ -> t b)

/// Functions for creating or using prisms.
[<RequireQualifiedAccess>]
module Prism =
  /// Converts an epimorphism into a prism.
  let ofEpimorphism ((f, t): Epimorphism<'a,'b>) : Prism<'a,'b> = f, (fun b _ -> t b)

/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an identity lens.
[<AutoOpen>]
module Optics =
  // Lens for the identity function (does not change the focus of operation).
  let id_ : Lens<'a,'a> =
    (fun x -> x),
    (fun x _ -> x)
  
  /// Isomorphism between a boxed and unboxed type.
  let box_<'a> : Isomorphism<obj,'a> =
    unbox<'a>, box
  
  /// Lens to the first item of a tuple.
  let fst_ : Lens<('a * 'b),'a> =
    fst,
    (fun a t -> a, snd t)
  
  /// Lens to the second item of a tuple.
  let snd_ : Lens<('a * 'b),'b> =
    snd,
    (fun b t -> fst t, b)
  
  [<RequireQualifiedAccess>]
  module Map =
    /// Prism to a value associated with a key in a map.
    let key_ (k: 'k) : Prism<Map<'k,'v>,'v> =
      Map.tryFind k,
      (fun v x -> if Map.containsKey k x then Map.add k v x else x)
    
    /// Lens to a value option associated with a key in a map.
    let value_ (k: 'k) : Lens<Map<'k,'v>, 'v option> =
      Map.tryFind k,
      (fun v x ->
        match v with
        | Some v -> Map.add k v x
        | _ -> Map.remove k x)

  [<RequireQualifiedAccess>]
  module Pojo =
    open Fable.Core.JsInterop

    [<Emit("Object.prototype.hasOwnProperty.call($1,$0)")>]
    let private jsHasProp (prop: string, o: obj): bool = jsNative
    [<Emit("$1[$0]")>]
    let private jsGetProp (prop: string, o: obj): 'a = jsNative
    [<Emit("Object.assign({},$0,$1)")>]
    let private jsAssign (obj1: obj, obj2: obj): obj = jsNative
    [<Emit("delete $1[$0]")>]
    let private jsDelete (prop: string, o: obj): unit = jsNative

    let private getProp (prop: string) (o: obj) =
      match jsHasProp (prop, o) with
      | true -> Some <| jsGetProp (prop, o)
      | false -> None
    
    let private copyAndDelete (prop: string) (o: obj) =
      let copy = jsAssign (o, createObj [])
      jsDelete (prop, copy)
      copy

    let prop_ (key: string): Prism<obj, obj option> =
      (getProp key),
      (fun v o ->
        match v with
        | Some v -> jsAssign (o, createObj [ key ==> v ])
        | None -> copyAndDelete key o)
  
/// Optional custom operators for working with optics. Provides more concise
/// syntactic options for working with the functions in the `Compose` and
/// `Optic` modules.
[<AutoOpen>]
module Operators =
  /// Compose a lens with an optic or morphism.
  let inline (>->) l o =
    Compose.lens l o
  
  /// Compose a prism with an optic or morphism.
  let inline (>?>) p o =
    Compose.prism p o
