
type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; } 

type LineSegment = {startPoint : Point; endPoint : Point; color : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)

type LogoCommand =
    | Forward of float * Color option
    | Turn of float
   
type LTurtle = 
    { angle : float
      x : float
      y : float } 

//let rec processTurtle turtle program =
//    