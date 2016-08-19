module Engine

open System
open EngineTypes
open EngineConsoleRender
open EngineInitializers
open EngineHyperSeriousCore.ComputeNextBoard

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
    let runNextEpochAction = fun _ -> (runNextEpoch stopper renderer (getNextBoard board) (currentEpoch + 1))
    renderer board
    match stopper with
    | KeyPressed -> wasKeyPressed ConsoleKey.Q runNextEpochAction
    | EpochMaxReached -> wasEpochMaxReached 5 currentEpoch runNextEpochAction

let startSimulation stopper boardSize =
    runNextEpoch stopper renderBoardInConsole (makeBoard boardSize) 0