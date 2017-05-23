module Fable.Import.Ava

open Fable.Core

type IAsserter =
  [<Emit("$0.pass({{$1}})")>]
  abstract Pass: ?msg: string -> unit
  [<Emit("$0.fail({{$1}})")>]
  abstract Fail: ?msg: string -> unit
  [<Emit("$0.true($1,{{$2}})")>]
  abstract True: value: bool * ?msg: string -> unit
  [<Emit("$0.false($1,{{$2}})")>]
  abstract False: value: bool * ?msg: string -> unit

[<Import("default", "ava")>]
[<Emit("$0($1,$2)")>]
let test (name: string) (test: IAsserter -> unit): unit = jsNative