#load "Domain.fsx"

open System

let header = """
<!DOCTYPE html>
<html>
<body>
<svg height="800" width="800">"""
 
let footer = """
</svg>
</body>
</html>
"""


let toSvg (ops:Domain.LineSegment seq) =
    let asString (segment: Domain.LineSegment)  =
        sprintf """<line x1="%i" y1="%i" x2="%i" y2="%i" style="stroke:rgb(%i,%i,%i);stroke-width:1" />""" segment.startPoint.x  segment.startPoint.y segment.endPoint.x segment.endPoint.y segment.color.r segment.color.g segment.color.b 

    [ yield header
      for op in ops -> asString op
      yield footer ]
    |> String.concat "\n"

open System.IO

let path = "../lsystem.html"
let save template = File.WriteAllText(path,template)

open Domain

let testProgram = 
    [for x in 1..8 do
         yield Forward(10.0,Some red)
         yield Forward(10.0,None)
         yield Turn 45.0 ]

let executeProgram program =
    let t = {angle = 0.0; x = 100.0; y = 100.0}
    (processTurtle t program)


//[<EntryPoint>]
let main   =
    lsystem "A" 5
    |> convertToTurtle
    |>  executeProgram
    |> toSvg 
    |> save
    0

main 
