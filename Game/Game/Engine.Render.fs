module EngineRender

open EngineTypes

let renderBoard (render: List<Row> -> unit) (board:List<Row>) = 
    render board

let renderReducer<'T, 'U> concat (render: 'T -> 'U) (acc: 'U) (el: 'T) : 'U =
   concat acc (render el)