module YoloDev.GitHub.Core.Test

open Fable.Import.Ava

test "test1" (fun Assert ->
  Assert.Pass "No problem"
  Assert.Pass "Nemo problemo"
)

test "test2" (fun Assert ->
  Assert.True (true, "I pass with flying colors :)")
  Assert.False false
)