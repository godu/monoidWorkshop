(* set *)
type integer = int;;
type boolean = bool;;
type shape =
  | Circle
  | Triangle
  | Square;;

let shapeValues = [Circle; Triangle; Square];;
let shapeToString shape =
  match shape with
  | Circle -> "Circle"
  | Triangle -> "Triangle"
  | Square -> "Square";;

(* Algebraic structure *)
type ('a, 'b) operation = 'a -> 'b;;

type ('a, 'b) algebraicStructure = {
  operation: ('a, 'b) operation
};;

let stringGetLengthAS: (string, int) algebraicStructure = {
  operation = String.length
};;

let log =
  "foo"
  |> stringGetLengthAS.operation

(* Closed binary operation *)
type ('a, 'b) binaryOperation = ('a, 'a -> 'b) operation;;
type 'a closedBinaryOperation = ('a, 'a) binaryOperation;;

let (+?): shape closedBinaryOperation = fun a b ->
  match (a, b) with
  | (Circle, Circle) -> Circle
  | (Circle, Triangle) -> Triangle
  | (Circle, Square) -> Square
  | (Triangle, Circle) -> Triangle
  | (Triangle, Triangle) -> Square
  | (Triangle, Square) -> Circle
  | (Square, Circle) -> Square
  | (Square, Triangle) -> Circle
  | (Square, Square) -> Triangle;;

let log =
  Circle +? Triangle;;

(* Magma *)
type 'a magma = {
  combine: 'a closedBinaryOperation
};;

(* let reduceMagma magma values =
  values
  |> List.reduce ~f:magma.combine;;

let shapeMagma: shape magma = {
  combine = (+?)
};;

let log =
  [Circle; Triangle; Square]
  |> reduceMagma shapeMagma;; *)

(* Associative property *)
let assertAssociativeProperty combine isEqual values =
  values
  |> List.for_all (fun a ->
    List.for_all (fun b ->
      List.for_all (fun c ->
        isEqual
          (combine (combine a b) c)
          (combine a (combine b c))
      ) values
    ) values
  );;

let combineSquareToRight _ = (+?) Square;;
let log =
  shapeValues
  |> assertAssociativeProperty combineSquareToRight (=);;

let log =
  shapeValues
  |> assertAssociativeProperty (+?) (=);;

let log =
  [1; 2; 3;]
  |> assertAssociativeProperty (-) (=);;

(* Semigroup *)
type 'a semigroup = 'a magma;;

let reduceSemigroup semigroup values =
  List.reduce values semigroup.combine;;

let listIntersectionMagma: shape list semigroup = {
  combine = fun a b -> List.concat [a; b]
};;

[
  [Circle; Triangle; Square];
  [Circle; Triangle; Square]
]
|> reduceSemigroup listIntersectionMagma;;

(* Identity element *)
let assertIdentityElement combine isEqual idElement values =
  List.for_all (fun a ->
    isEqual (combine a idElement) a
  ) values

let rightPick _ a = a
let log = assertIdentityElement rightPick (=) Circle shapeValues
let log = assertIdentityElement (+?) (=) Circle shapeValues

(* Monoid *)
type 'a monoid = 'a semigroup & {
  empty: 'a
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
