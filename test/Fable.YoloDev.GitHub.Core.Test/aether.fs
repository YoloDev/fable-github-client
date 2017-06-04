module YoloDev.GitHub.Core.Test

open Fable.Ava
open Fable.Ava.TestCheck

module Assert =
  let isEqual a b msg = prop {
    if (a = b) then
      do! Assert.pass <?> msg
    else
      do! Assert.isDeepEqual a b <?> msg
  }

/// Properties that can be used to evaluate the conformance of Lenses, Prisms,
/// Isomorphisms, and Epimorphisms to certain invariants.
module Properties =
  /// Lens properties
  [<RequireQualifiedAccess>]
  module Lens =
    let get (g, _) = fun o -> g o
    let set (_, s) = fun i o -> s i o
    let map (g, s) = fun f o -> s (f (g o)) o

    let getSetIdentityWith lensGet lensSet outer = prop {
      do! Assert.isEqual (lensSet (lensGet outer) outer) outer "Get-Set Identity"
    }

    let setGetSymmetryWith lensGet lensSet outer inner = prop {
      do! Assert.isEqual (lensGet (lensSet inner outer)) inner "Set-Get Symmetry"
    }

    let setSetOrderDependenceWith lensSet outer inner dummy = prop {
      if inner = dummy then
        do! Assert.pass <?> "Trivial case"
      else
        do! Assert.isEqual (lensSet inner (lensSet dummy outer)) (lensSet inner outer) "Set-Set Order Dependence"
    }

    let getSetMapCorrespondenceWith lensGet lensSet lensMap mapFn outer = prop {
      do! Assert.isEqual (lensSet (lensGet outer |> mapFn) outer) (lensMap mapFn outer) "Get-Set to Map Correspondence"
    }

    let unwrapLens f lens =
      f (get lens) (set lens)

    /// The Get-Set Identity requires that modifying an entity through a lens by setting a value
    /// to exactly what it was before then nothing happens.
    let getSetIdentity lens = unwrapLens getSetIdentityWith lens

    /// The Set-Get Symmetry requires that modifying an entity through a lens by setting a value
    /// and then viewing that same value returns the value that was just set through it.
    let setGetSymmetry lens = unwrapLens setGetSymmetryWith lens

    /// The Set-Set Order Dependence requires that modifying an entity through a lens by setting
    /// a value to `a` and then setting the same value to `b` behaves the same as having modified
    /// the original entity by only setting the value to `b`.
    let setSetOrderDependence lens = setSetOrderDependenceWith (set lens)

    /// The Get-Set to Map Corresponsence requires that modifying an entity through a lens
    /// by getting a value, applying some function `f` to that value and then setting the value to the result
    /// behaves the same as having mapped that function through the lens.
    let inline getSetMapCorrespondence lens = unwrapLens getSetMapCorrespondenceWith lens <| map lens

    /// Requires that a lens follows the Lens Laws and is well-behaved:
    ///
    /// * Get-Set Identity,
    /// * Set-Get Symmetry, and
    /// * Set-Set Order Dependence.
    ///
    /// Is "Trivial" when `inner = dummy`.
    let followsLensLaws lens outer inner dummy mapFn = prop {
      if inner = dummy then
        do! Assert.pass
        do! Assert.pass
        do! Assert.pass
        do! Assert.pass
      else
        do! getSetIdentity lens outer
        do! setGetSymmetry lens outer inner
        do! setSetOrderDependence lens outer inner dummy
        do! getSetMapCorrespondence lens mapFn outer
    }

  /// Prism properties
  [<RequireQualifiedAccess>]
  module Prism =
    let get (g, _) = fun o -> g o
    let set (_, s) = fun i o -> s i o
    let map (g, s) = fun f o -> Option.map f (g o) |> function | Some i -> s i o | _ -> o

    let getSetIdentityWith prismGet prismSet outer dummy = prop {
      do! Assert.isEqual (prismSet (defaultArg (prismGet outer) dummy) outer) outer "Get-Set Identity"
    }

    let setGetSymmetryWith prismGet prismSet outer inner = prop {
      match prismGet outer with
      | Some _ -> do! Assert.isEqual (prismSet inner outer |> prismGet) (Some inner) "Set-Get Symmetry - inner should be changed"
      | None   -> do! Assert.isEqual (prismSet inner outer) outer "Set-Get Symmetry - outer should remain unchanged"
    }

    let setSetOrderDependenceWith prismGet prismSet outer inner dummy = prop {
      if inner = dummy then
        do! Assert.pass
      else
        do! Assert.isEqual (prismSet inner outer) (prismSet inner (prismSet dummy outer)) "Set-Set Order Dependence"
    }

    let getSetMapCorrespondenceWith prismGet prismSet prismMap f outer = prop {
      do! Assert.isEqual (prismMap f outer) (prismGet outer |> function | Some i -> prismSet (f i) outer | None -> outer) "Get-Set to Map Correspondence"
    }

    let inline unwrapPrism f prism =
      f (get prism) (set prism)

    /// The Get-Set Identity requires that modifying an entity through a prism by setting a value
    /// to exactly what it was before then nothing happens.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    let inline getSetIdentity prism = unwrapPrism getSetIdentityWith prism

    /// The Set-Get Symmetry requires that modifying an entity through a prism by setting a value
    /// and then viewing that same value returns either the value that was just set through it. If
    /// the prism doesn't resolve to `Some`, then the entity should not be modified.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    let inline setGetSymmetry prism = unwrapPrism setGetSymmetryWith prism

    /// The Set-Set Order Dependence requires that modifying an entity through a prism by setting
    /// a value to `a` and then setting the same value to `b` behaves the same as having modified
    /// the original entity by only setting the value to `b`.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    /// Is "Trivial" when `inner = dummy`.
    let inline setSetOrderDependence prism = unwrapPrism setSetOrderDependenceWith prism

    /// The Get-Set to Map Corresponsence requires that modifying an entity through a prism
    /// by getting a value, applying some function `f` to that value and then setting the value to the result
    /// behaves the same as having mapped that function through the prism.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    let inline getSetMapCorrespondence prism = unwrapPrism getSetMapCorrespondenceWith prism <| map prism

    /// Requires that a prism follows the Prism Laws and is well-behaved:
    /// * Get-Set Identity,
    /// * Set-Get Symmetry, and
    /// * Set-Set Order Dependence.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    /// Is "Trivial" when `inner = dummy`.
    let followsPrismLaws prism outer inner dummy f = prop {
      if inner = dummy then
        do! Assert.pass
        do! Assert.pass
        do! Assert.pass
        do! Assert.pass
      else
        do! getSetIdentity prism outer dummy
        do! setGetSymmetry prism outer inner
        do! setSetOrderDependence prism outer inner dummy
        do! getSetMapCorrespondence prism f outer
    }

  /// Isomorphism properties
  [<RequireQualifiedAccess>]
  module Isomorphism =
    let asLens (f, t) = f, (fun c _ -> t c)

    /// Requires that mapping a value through an isomorphism and then mapping it
    /// back returns a value equivalent to the original.
    let roundtripEquality iso outer = prop {
      do! Assert.isEqual (fst iso outer |> snd iso) outer "Roundtrip Equality"
    }

    /// Requires that conversely mapping a value through an isomorphism and then
    /// mapping it back returns a value equivalent to the original.
    let converseRoundtripEquality iso inner = prop {
      do! Assert.isEqual (snd iso inner |> fst iso) inner "Converse Roundtrip Equality"
    }

    /// Requires that an isomorphism demonstrates certain properties to ensure
    /// unidirectional isomorphic operations are sane.
    let followsWeakIsomorphismLaws iso outer inner dummy = prop {
      let isoAsLens = asLens iso
      do! Lens.getSetIdentity isoAsLens outer
      do! Lens.setSetOrderDependence isoAsLens outer inner dummy
      do! roundtripEquality iso outer
    }

    /// Requires that an isomorphism demonstrates certain properties to ensure
    /// isomorphic operations in either direction are sane.
    let followsIsomorphismLaws iso outer inner dummy f = prop {
      do! Lens.followsLensLaws (asLens iso) outer inner dummy f
      do! roundtripEquality iso outer
      do! converseRoundtripEquality iso inner
    }

  /// Epimorphism properties
  [<RequireQualifiedAccess>]
  module Epimorphism =
    let asPrism (f, t) = f, (fun c _ -> t c)

    /// Requires that, if mapping a value through an epimorphism results in a value,
    /// mapping that value back returns a value equivalent to the original.
    let roundtripEquality epi outer = prop {
      match fst epi outer with
      | None -> do! Assert.pass
      | Some _ -> do! Assert.isEqual (fst epi outer |> Option.map (snd epi)) (Some outer) "Roundtrip Equality"
    }

    /// Requires that conversely mapping a value through an epimorphism and then
    /// mapping it back returns a value equivalent to the original.
    let converseRoundtripEquality epi inner = prop {
      do! Assert.isEqual (snd epi inner |> fst epi) (Some inner) "Converse Roundtrip Equality"
    }

    /// Requires that an epimorphism demonstrates minimal properties to ensure
    /// sane operations in one direction.
    let followsWeakEpimorphismLaws epi outer inner dummy = prop {
      do! Prism.setSetOrderDependence (asPrism epi) outer inner dummy
      do! roundtripEquality epi outer
    }

    /// Requires that an epimorphism demonstrates minimal properties to ensure
    /// sane operations in both directions.
    let followsEpimorphismLaws epi outer inner dummy = prop {
      do! Prism.setSetOrderDependence (asPrism epi) outer inner dummy
      do! roundtripEquality epi outer
      do! converseRoundtripEquality epi inner
    }

open System
open YoloDev.GitHubClient.Core.Aether

[<AutoOpen>]
module Data =
  let chars : Isomorphism<string, char[]> =
    (fun x -> x.ToCharArray ()), (fun x -> String (x))

  let rev : Isomorphism<char[], char[]> =
    Array.rev, Array.rev

  let times2 = (*) 2
  let maybeInt = function
    | Some x when x % 2 = 0 -> Some (x * 3)
    | Some _ -> None
    | None -> Some 1

  let maybeBoxedInt =
    Option.map unbox
    >> maybeInt
    >> Option.map box

// Generators
//

let gInt = Generator.int
let gPosInt = Generator.posInt
let gAlphaNum = Generator.alphaNumString

let gOption = Generator.optional
let gBox<'a> : Generator<'a> -> Generator<obj> = Generator.map box

let gShortList<'a> : Generator<'a> -> Generator<'a list> =
  Generator.listWithOptions (SizeOptions.max 2)

let gShortArray<'a> : Generator<'a> -> Generator<'a array> =
  Generator.arrayWithOptions (SizeOptions.max 2)

let gTuple2' g = Generator.tuple2 g g

let gChoice2 g1 g2 =
  let gChoice1 = g1 |> Generator.map Choice1Of2
  let gChoice2 = g2 |> Generator.map Choice2Of2
  Generator.oneOf [gChoice1; gChoice2]

let gResult g1 g2 =
  let gOk = g1 |> Generator.map Ok
  let gError = g2 |> Generator.map Error
  Generator.oneOf [gOk; gError]

let gStringIntList =
  Generator.tuple2 gAlphaNum gInt
  |> Generator.list

let gStringIntArray =
  Generator.tuple2 gAlphaNum gInt
  |> Generator.array

let gStringMap g =
  Generator.tuple2 gAlphaNum g
  |> Generator.list
  |> Generator.map Map.ofList

// TODO: Replace with object generators once implemented
let gPojo g =
  Generator.tuple2 Generator.alphaNumString (gBox g)
  |> Generator.list
  |> Generator.map Fable.Core.JsInterop.createObj

// Helpers
//

module Prop =
  let create3 (name: string) g1 g2 g3 fn =
    Prop.create name (Generator.tuple3 g1 g2 g3) (fun (a, b, c) -> fn a b c)

  let create4 (name: string) g1 g2 g3 g4 fn =
    Prop.create name (Generator.tuple4 g1 g2 g3 g4) (fun (a, b, c, d) -> fn a b c d)

  let inline create' (name: string) g fn = create3 name g gInt gInt fn

// Tests
//

// Lenses
//

Prop.create' "builtins > id_ follows the Lens Laws" gInt <|
  fun outer inner dummy -> Properties.Lens.followsLensLaws id_ outer inner dummy times2

Prop.create' "builtins > fst_ follows the Lens Laws" (gTuple2' gInt) <|
  fun outer inner dummy -> Properties.Lens.followsLensLaws fst_ outer inner dummy times2

Prop.create' "builtins > snd_ follows the Lens Laws" (gTuple2' gInt) <|
  fun outer inner dummy -> Properties.Lens.followsLensLaws snd_ outer inner dummy times2

Prop.create4 "builtins > Map.value_ follows the Lens Laws" gAlphaNum (gStringMap gInt) (gOption gInt) (gOption gInt) <|
  fun key outer inner dummy -> Properties.Lens.followsLensLaws (Map.value_ key) outer inner dummy maybeInt

Prop.create4 "builtins > Pojo.value_ follows the Lens Laws" gAlphaNum (gPojo gInt) (gOption (gBox gInt)) (gOption (gBox gInt)) <|
  fun key outer inner dummy -> Properties.Lens.followsLensLaws (Pojo.value_ key) outer inner dummy maybeBoxedInt


// Prisms
//

Prop.create' "builtins > Choice.choice1Of2_ follows the Prism Laws" (gChoice2 gInt gInt) <|
  fun outer inner dummy -> Properties.Prism.followsPrismLaws Choice.choice1Of2_ outer inner dummy times2

Prop.create' "builtins > Choice.choice2Of2_ follows the Prism Laws" (gChoice2 gInt gInt) <|
  fun outer inner dummy -> Properties.Prism.followsPrismLaws Choice.choice2Of2_ outer inner dummy times2

Prop.create' "builtins > Result.ok_ follows the Prism Laws" (gResult gInt gInt) <|
  fun outer inner dummy -> Properties.Prism.followsPrismLaws Result.ok_ outer inner dummy times2

Prop.create' "builtins > Result.error_ follows the Prism Laws" (gResult gInt gInt) <|
  fun outer inner dummy -> Properties.Prism.followsPrismLaws Result.error_ outer inner dummy times2

Prop.create' "builtins > Option.value_ follows the Prism Laws" (gOption gInt) <|
  fun outer inner dummy -> Properties.Prism.followsPrismLaws Option.value_ outer inner dummy times2

Prop.create' "builtins > List.head_ follows the Prism Laws" (gShortList gInt) <|
  fun outer inner dummy -> Properties.Prism.followsPrismLaws List.head_ outer inner dummy times2

Prop.create3 "builtins > List.tail_ follows the Prism Laws" (gShortList gInt) (gShortList gInt) (gShortList gInt) <|
  fun outer inner dummy -> Properties.Prism.followsPrismLaws List.tail_ outer inner dummy (List.map times2)

Prop.create4 "builtins > List.pos_ follows the Prism Laws" gPosInt (gShortList gInt) gInt gInt <|
  fun idx outer inner dummy -> Properties.Prism.followsPrismLaws (List.item_ idx) outer inner dummy times2

Prop.create4 "builtins > Map.key_ follows the Prism Laws" gAlphaNum (gStringMap gInt) gInt gInt <|
  fun key outer inner dummy -> Properties.Prism.followsPrismLaws (Map.key_ key) outer inner dummy int

Prop.create4 "builtins > Pojo.value_ follows the Prism Laws" gAlphaNum (gPojo gInt) (gBox gInt) (gBox gInt) <|
  fun key outer inner dummy -> Properties.Prism.followsPrismLaws (Pojo.key_ key) outer inner dummy unbox

// Morphisms
//

Prop.create3 "builtins > Map.list_ follows the Weak Isomorphism Laws" (gStringMap gInt) gStringIntList gStringIntList <|
  fun outer inner dummy -> Properties.Isomorphism.followsWeakIsomorphismLaws Map.list_ outer inner dummy

Prop.create3 "builtins > Map.array_ follows the Weak Isomorphism Laws" (gStringMap gInt) gStringIntArray gStringIntArray <|
  fun outer inner dummy -> Properties.Isomorphism.followsWeakIsomorphismLaws Map.array_ outer inner dummy

Prop.create3 "builtins > Array.list_ follows the Isomorphism Laws" (gShortArray gInt) (gShortList gInt) (gShortList gInt) <|
  fun outer inner dummy -> Properties.Isomorphism.followsIsomorphismLaws Array.list_ outer inner dummy (List.map times2)

Prop.create3 "builtins > List.array_ follows the Isomorphism Laws" (gShortList gInt) (gShortArray gInt) (gShortArray gInt) <|
  fun outer inner dummy -> Properties.Isomorphism.followsIsomorphismLaws List.array_ outer inner dummy (Array.map times2)

Prop.create3 "builtins > Choice.choice1Of2_ mapped through Map(toList/ofList) as a partial isomorphism follows the Weak Partial Isomorphism Laws" (gChoice2 (gStringMap gInt) gInt) gStringIntList gStringIntList <|
  fun outer inner dummy -> Properties.Epimorphism.followsWeakEpimorphismLaws ((fst Choice.choice1Of2_ >> Option.map Map.toList),(Map.ofList >> Choice1Of2)) outer inner dummy

Prop.create3 "builtins > Choice.choice1Of2_ as a partial isomorphism follows the Partial Isomorphism Laws" (gChoice2 gInt gInt) gInt gInt <|
  fun outer inner dummy -> Properties.Epimorphism.followsEpimorphismLaws (fst Choice.choice1Of2_,Choice1Of2) outer inner dummy


// Examplar Usage Tests
//

type MapExample = { map : Map<string, string> }
module MapExample =
  let map_ =
    (fun x -> x.map),
    (fun v x -> { x with map = v })

Test.create "Upserting into a Map using a Lens" <| prop {
  let example = { map = Map.ofList ["TestKey", "TestValue"] }
  let newValue = Optic.map MapExample.map_ (Map.add "TestKey2" "OtherValue") example
  do! Assert.isEqual newValue.map.["TestKey"] "TestValue" "Old key"
  do! Assert.isEqual newValue.map.["TestKey2"] "OtherValue" "New key"
}
