module Game

open Engine.Simulation

[<EntryPoint>]
let main argv =
    printfn "Starting simulation..."
    startFiniteSimulation 20
    0
