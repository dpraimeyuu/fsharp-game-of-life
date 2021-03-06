module EngineConsoleRender

open EngineTypes
open EngineRender
open System

let renderCellInConsole cell =
    match cell.alive with
    | Alive -> "*"
    | Dead -> "-"

let stringConcat (i: string) (j: string) : string =
    i + j

let renderCellInConsoleReducer = renderReducer<Cell, string> stringConcat renderCellInConsole

let renderRowInConsole row =
    let renderedView = "\n"
    row.cells
        |> List.fold renderCellInConsoleReducer renderedView

let renderRowInConsoleReducer = renderReducer<Row, string> stringConcat renderRowInConsole

let renderInConsole board =
    board
        |> List.fold renderRowInConsoleReducer String.Empty
        |> printfn "%s"

let renderBoardInConsole onBeforeRender board = 
    onBeforeRender ()
    renderBoard renderInConsole board

let renderBoardInConsoleClearedOutput = renderBoardInConsole (fun _ -> Console.Clear())
let renderBoardInConsolePersistedOutput = renderBoardInConsole id