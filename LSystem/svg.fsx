#load "Domain.fsx"

open System
open System.IO
open Domain

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

let path = "../lsystem.html"
let save template = File.WriteAllText(path,template)

let main   =
    ferns
    |> processLsystem 8
    |> (processTurtle {angle = 0.0; x = 0.0; y = (10.0) ; c = red}) 
    |> toSvg 
    |> save
    0

main 
