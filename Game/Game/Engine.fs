module Engine

open System
open EngineTypes
open EngineConsoleRender
open EngineInitializers
open EngineHyperSeriousCore.ComputeNextBoard
open System.Threading

let createTimer timerInterval action =
    let timer = new System.Timers.Timer(timerInterval)
    timer.AutoReset <- true
    timer.Elapsed.Add action
    async {
        timer.Start()
        do! Async.Sleep Timeout.Infinite
        }
        
let wasEpochMaxReached epochMaxNumber currentEpoch action =
    match currentEpoch with
    | currentEpoch when currentEpoch = epochMaxNumber -> ()
    | _ -> action ()

let rec runNextEpoch stopper (renderer: List<Row> -> unit) (board:List<Row>) currentEpoch =
    let runNextEpochAction = fun _ -> (runNextEpoch stopper renderer (getNextBoard board) (currentEpoch + 1))
    renderer board
    match stopper with
    | EpochMaxReached -> wasEpochMaxReached 5 currentEpoch runNextEpochAction
    | _ -> ()

let startInfiniteMode action =
    let task = createTimer 1000. action
    Async.RunSynchronously task

let startSimulation stopper boardSize =
    match stopper with
    | Infinite -> startInfiniteMode (fun _ ->
        runNextEpoch stopper renderBoardInConsoleClearedOutput (makeBoard boardSize) 0)
    | EpochMaxReached -> runNextEpoch stopper renderBoardInConsolePersistedOutput (makeBoard boardSize) 0