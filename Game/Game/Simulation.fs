module Simulation

open Engine
open EngineTypes

let startFiniteSimulation = startSimulation EpochMaxReached
let startInifiniteSimulation = startSimulation KeyPressed