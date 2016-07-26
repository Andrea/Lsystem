
type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; a:byte } 

type LineSegment = {startPoint : Point; endPoint : Point; color : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)

type LOGO =
    | Forward of float * Color option
    | Turn of float
    