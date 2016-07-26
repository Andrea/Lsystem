
type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; a:byte } 

type LineSegment = {startPoint : Point; endPoint : Point; colour : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)
