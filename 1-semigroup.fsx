// Set
type Integer = int
type String = string
type Boolean = bool
type Shape = Circle | Triangle | Square

let shapeValues = [Circle; Triangle; Square]

// Algebraic structure
type Operation<'T, 'U> = 'T -> 'U
type AlgebraicStructure<'T, 'U> = {
  operation: Operation<'T, 'U>
}

let stringGetLengthAS: AlgebraicStructure<String, Integer> = {
  operation = String.length
}

stringGetLengthAS.operation "foo"
|> printfn "stringGetLengthAS: %O"

// Closed binary operation
type BinaryOperation<'T, 'U> = Operation<'T, 'T -> 'U>
type ClosedBinaryOperation<'T> = BinaryOperation<'T, 'T>

let (+?): ClosedBinaryOperation<Shape> = fun a b ->
  match (a, b) with
  | (Circle, Circle) -> Circle
  | (Circle, Triangle) -> Triangle
  | (Circle, Square) -> Square
  | (Triangle, Circle) -> Triangle
  | (Triangle, Triangle) -> Square
  | (Triangle, Square) -> Circle
  | (Square, Circle) -> Square
  | (Square, Triangle) -> Circle
  | (Square, Square) -> Triangle

Circle +? Triangle
|> printfn "combineShape: %O"

// Magma
type Magma<'T> = { combine: ClosedBinaryOperation<'T> }

let reduceMagma magma values =
  List.reduce magma.combine values


let shapeMagma: Magma<Shape> = {
  combine = (+?)
}

reduceMagma shapeMagma shapeValues
|> printfn "shapeMagma: %O"

// Associative property
let assertAssociativeProperty combine isEqual values =
  List.forall (fun a ->
    List.forall (fun b ->
      List.forall (fun c ->
        if (isEqual (combine (combine a b) c) (combine a (combine b c)))
        then
          true
        else
          printfn
              "%O  %O <> %O"
              [a; b; c]
              (combine (combine a b) c)
              (combine a (combine b c))
          false
      ) values
    ) values
  ) values

let combineSquareToRight _ = (+?) Square
assertAssociativeProperty combineSquareToRight (=) shapeValues
|> printfn "assertAssociativeProperty pickLeft: %O"
assertAssociativeProperty (+?) (=) shapeValues
|> printfn "assertAssociativeProperty +?: %O"

// Semigroup
type Semigroup<'T> = Magma<'T>

let reduceSemigroup semigroup values =
  List.reduce semigroup.combine values

let listIntersectionMagma: Semigroup<Set<Shape>> = {
  combine = Set.intersect
}

[
  Set.ofList [Circle; Triangle];
  Set.ofList [Triangle; Square]
]
|> reduceSemigroup listIntersectionMagma
|> printfn "shapeMagma: %O"

// Identity element
let assertIdentityElement combine isEqual idElement values =
  List.forall (fun a ->
    if (isEqual (combine a idElement) a)
    then
      true
    else
      printfn
          "%O"
          a
      false
  ) values

let rightPick _ a = a
assertIdentityElement rightPick (=) Circle shapeValues
|> printfn "assertIdentityElement pickLeft: %O"
assertIdentityElement (+?) (=) Circle shapeValues
|> printfn "assertIdentityElement +?: %O"

// Monoid
type Monoid<'T> = {
  combine: 'T -> 'T -> 'T
  empty: 'T
}

let reduceMonoid monoid values =
  List.fold monoid.combine monoid.empty values

let shapeMonoid: Monoid<Shape> = {
  combine = (+?)
  empty = Circle
}

[ Circle; Triangle; Square; ]
|> reduceMonoid shapeMonoid
|> printfn "shapeMonoid: %O"

// Parallelization
open System.Linq

let parallelReduceMonoid monoid (values: _[]) =
  values.AsParallel().Aggregate(monoid.empty, monoid.combine)

[| Circle; Triangle; Square; |]
|> parallelReduceMonoid shapeMonoid
|> printfn "shapeMonoid: %O"
