module Engine

open System

type Point = {x: int; y: int}
type CellState = Alive | Dead
type Cell = {alive: CellState; coordinates: Point}
type Row = {rowNumber: int; cells: List<Cell>}
type SimulationStopper = KeyPressed | EpochMaxReached

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

let updateCellState cell nextCellState =
    {cell with alive = nextCellState}

let updateRow (nextCellStates:List<CellState>) (row:Row) : Row =
    let updatedCells = List.zip row.cells nextCellStates
                         |> List.map (fun (cell, state) -> updateCellState cell state)
    {row with cells = updatedCells}

let permuteToPoints (xs:List<int>) (ys:List<int>) : List<Point> = 
    xs
        |> List.collect (fun x -> ys |> List.map(fun y -> {x = x; y = y}))
        |> List.filter (fun p -> not (p.x = 0 && p.y = 0))

let ranges = permuteToPoints [-1;0;1] [-1;0;1]

let getCell (board:List<Row>) (point:Point) : Cell =
    board
        |> List.filter (fun row -> row.rowNumber = point.y)
        |> List.map (fun row -> row.cells)
        |> List.head
        |> List.filter (fun cell -> cell.coordinates.x = point.x)
        |> List.head

let filterOutOfBounds bound points =
    points
        |> List.filter (fun p -> (p.x >= 0 && p.x < bound) && (p.y >= 0 && p.y < bound))

let getNeighbours (offsets:List<Point>) (board:List<Row>) (cell:Cell) : List<Cell> =
    offsets
        |> List.map (fun offset -> {x = offset.x + cell.coordinates.x; y = offset.y + cell.coordinates.y})
        |> filterOutOfBounds (board |> List.length)
        |> List.map (fun point -> getCell board point)

let get8Neighbours = getNeighbours ranges

let getNextCellState (board:List<Row>) (cell:Cell) : CellState = 
    let aliveNeighboursNumber = get8Neighbours board cell
                                |> List.filter (fun n -> n.alive = Alive)
                                |> List.fold (fun acc el -> acc + 1) 0
    match aliveNeighboursNumber with
    | 2 -> Alive
    | 3 -> Alive
    | _ -> Dead

let getNextCellStates board = 
    board
        |> List.map (fun row -> row.cells)
        |> List.map (fun cells -> cells |> List.map (fun cell -> getNextCellState board cell))

let updateBoard (board:List<Row>) : List<Row> =
    let nextCellStatesMatrix = getNextCellStates board
    List.zip nextCellStatesMatrix board
        |> List.map (fun (nextCellStates, row) -> updateRow nextCellStates row)

let renderBoard render board = 
    render board

let renderReducer render acc el =
    acc + render el

let renderCellInConsole cell =
    match cell.alive with
    | Alive -> "*"
    | Dead -> "-"

let renderCellInConsoleReducer = renderReducer renderCellInConsole

let renderRowInConsole row =
    let renderedView = "\n"
    row.cells
        |> List.fold renderCellInConsoleReducer renderedView

let renderRowInConsoleReducer = renderReducer renderRowInConsole

let renderInConsole board =
    board
        |> List.fold renderRowInConsoleReducer String.Empty
        |> printfn "%s"

let renderBoardInConsole = renderBoard renderInConsole

let wasKeyPressed terminationKey action =
    match Console.KeyAvailable with
    | true -> match Console.ReadKey().Key with
              | key when terminationKey = key -> ()
              | _ -> action ()
    | false -> action ()

let wasEpochMaxReached epochMaxNumber currentEpoch action =
    match currentEpoch with
    | currentEpoch when currentEpoch = epochMaxNumber -> ()
    | _ -> action ()


let rec runNextEpoch stopper (renderer: List<Row> -> unit) (board:List<Row>) currentEpoch =
    let runNextEpochAction = fun _ -> (runNextEpoch stopper renderer (updateBoard board) (currentEpoch + 1))
    renderer board
    match stopper with
    | KeyPressed -> wasKeyPressed ConsoleKey.Q runNextEpochAction
    | EpochMaxReached -> wasEpochMaxReached 5 currentEpoch runNextEpochAction

let startSimulation stopper boardSize =
    runNextEpoch stopper renderBoardInConsole (makeBoard boardSize) 0

module Simulation =

    let startFiniteSimulation = startSimulation EpochMaxReached
    let startInifiniteSimulation = startSimulation KeyPressed