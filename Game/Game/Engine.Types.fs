module EngineTypes

type Point = {x: int; y: int}
type CellState = Alive | Dead
type Cell = {alive: CellState; coordinates: Point}
type Row = {rowNumber: int; cells: List<Cell>}
type SimulationStopper = KeyPressed | EpochMaxReached