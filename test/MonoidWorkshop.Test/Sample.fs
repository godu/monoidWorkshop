module Tests

open Expecto
open MonoidWorkshop

[<Tests>]
let tests =
  testList "samples" [
    testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
      let actual = Say.hello "foo"
      let expected = "Hello foo"
      Expect.equal actual expected "yolo"
  ]
