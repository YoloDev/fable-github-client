module YoloDev.GitHub.Core.Test

open Fable.Import.Ava

test "test1" <| fun t ->
  t.Pass "No problem"

test "test2" <| fun t ->
  t.True (true, "I pass with flying colors :)")