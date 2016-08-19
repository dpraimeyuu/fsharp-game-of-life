module EngineInitializers

open System
open EngineTypes

let getRandomNumber (rand:System.Random) =
    rand.NextDouble()

let generateAliveWhen (alivePredicate: float -> bool) (randomFloat:float) : CellState = 
    match alivePredicate randomFloat with
    | true -> Alive
    | false -> Dead

let random = System.Random
let generateAliveWhenAboveHalf = generateAliveWhen (fun num -> num > 0.5)
let rand = System.Random()

let makeRow rowLength rowNumber = 
    {rowNumber = rowNumber; cells = [0..rowLength - 1] |> List.map (fun i -> {alive = generateAliveWhenAboveHalf (getRandomNumber rand); coordinates = {x = i; y = rowNumber}}) }

let makeBoard numberOfTilesInRow : List<Row> = 
    [0..numberOfTilesInRow - 1]
        |> List.map (fun i -> makeRow numberOfTilesInRow i)

let permuteToPoints (xs:List<int>) (ys:List<int>) : List<Point> = 
    xs
        |> List.collect (fun x -> ys |> List.map(fun y -> {x = x; y = y}))
        |> List.filter (fun p -> not (p.x = 0 && p.y = 0))

let ranges = permuteToPoints [-1;0;1] [-1;0;1]
