module Engine

open System
open EngineTypes
open EngineConsoleRender
open EngineInitializers


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

let updateCellState cell nextCellState =
    {cell with alive = nextCellState}

let updateRow (nextCellStates:List<CellState>) (row:Row) : Row =
    let updatedCells = List.zip row.cells nextCellStates
                         |> List.map (fun (cell, state) -> updateCellState cell state)
    {row with cells = updatedCells}
    
let updateBoard (board:List<Row>) : List<Row> =
    let nextCellStatesMatrix = getNextCellStates board
    List.zip nextCellStatesMatrix board
        |> List.map (fun (nextCellStates, row) -> updateRow nextCellStates row)

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