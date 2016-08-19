module Game

open Simulation

[<EntryPoint>]
let main argv =
    printfn "Starting simulation..."
    startInifiniteSimulation 20
    0
