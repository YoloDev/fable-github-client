module YoloDev.GitHub.Core.Test

open Fable.Ava
open Fable.Ava.TestCheck

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
      do! Assert.isDeepEqual (lensSet (lensGet outer) outer) outer <?> "Get-Set Identity"
    }

    let setGetSymmetryWith lensGet lensSet outer inner = prop {
      do! Assert.isDeepEqual (lensGet (lensSet inner outer)) inner <?> "Set-Get Symmetry"
    }

    let setSetOrderDependenceWith lensSet outer inner dummy = prop {
      if inner = dummy then
        do! Assert.pass <?> "Trivial case"
      else
        do! Assert.isDeepEqual (lensSet inner (lensSet dummy outer)) (lensSet inner outer) <?> "Set-Set Order Dependence"
    }

    let getSetMapCorrespondenceWith lensGet lensSet lensMap mapFn outer = prop {
      do! Assert.isDeepEqual (lensSet (lensGet outer |> mapFn) outer) (lensMap mapFn outer) <?> "Get-Set to Map Correspondence"
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
    let getSetMapCorrespondence lens = unwrapLens getSetMapCorrespondenceWith lens (map lens)

    /// Requires that a lens follows the Lens Laws and is well-behaved:
    /// 
    /// * Get-Set Identity,
    /// * Set-Get Symmetry, and
    /// * Set-Set Order Dependence.
    /// 
    /// Is "Trivial" when `inner = dummy`.
    let followsLensLaws lens outer inner dummy mapFn = prop {
      if inner = outer then
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
      do! Assert.isDeepEqual (prismSet (defaultArg (prismGet outer) dummy) outer) outer <?> "Get-Set Identity"
    }

    let setGetSymmetryWith prismGet prismSet outer inner = prop {
      match prismGet outer with
      | Some _ -> do! Assert.isDeepEqual (prismSet inner outer |> prismGet) (Some inner) <?> "Set-Get Symmetry - inner should be changed"
      | None   -> do! Assert.isDeepEqual (prismSet inner outer) outer <?> "Set-Get Symmetry - outer should remain unchanged"
    }

    let setSetOrderDependenceWith prismGet prismSet outer inner dummy = prop {
      if inner = outer then
        do! Assert.pass
      else
        do! Assert.isDeepEqual (prismSet inner outer) (prismSet inner (prismSet dummy outer)) <?> "Set-Set Order Dependence"
    }

    let getSetMapCorrespondenceWith prismGet prismSet prismMap f outer = prop {
      do! Assert.isDeepEqual (prismMap f outer) (prismGet outer |> function | Some i -> prismSet (f i) outer | None -> outer) <?> "Get-Set to Map Correspondence"
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
    let inline getSetMapCorrespondence prism = unwrapPrism getSetMapCorrespondenceWith prism (map prism)

    /// Requires that a prism follows the Prism Laws and is well-behaved:
    /// * Get-Set Identity,
    /// * Set-Get Symmetry, and
    /// * Set-Set Order Dependence.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    /// Is "Trivial" when `inner = dummy`.
    let followsPrismLaws prism outer inner dummy f = prop {
      if inner = outer then
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
      do! Assert.isDeepEqual (fst iso outer |> snd iso) outer <?> "Roundtrip Equality"
    }

    /// Requires that conversely mapping a value through an isomorphism and then
    /// mapping it back returns a value equivalent to the original.
    let converseRoundtripEquality iso inner = prop {
      do! Assert.isDeepEqual (snd iso inner |> fst iso) inner <?> "Converse Roundtrip Equality"
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
      | Some _ -> do! Assert.isDeepEqual (fst epi outer |> Option.map (snd epi)) (Some outer) <?> "Roundtrip Equality"
    }

    /// Requires that conversely mapping a value through an epimorphism and then
    /// mapping it back returns a value equivalent to the original.
    let converseRoundtripEquality epi inner = prop {
      do! Assert.isDeepEqual (snd epi inner |> fst epi) (Some inner) <?> "Converse Roundtrip Equality"
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

module Prop =
  let create3 (name: string) (g1: Generator<'a>) (g2: Generator<'b>) (g3: Generator<'c>) (fn: 'a -> 'b -> 'c -> SyncSpec<unit>) =
    Prop.create name (Generator.tuple3 g1 g2 g3) (fun (a, b, c) -> fn a b c)

Prop.create3 "builtins > id_ follows the Lens Laws" Generator.int Generator.int Generator.int <|
  fun outer inner dummy -> Properties.Lens.followsLensLaws id_ outer inner dummy times2