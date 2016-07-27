module Domain

type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; } 

type LineSegment = {startPoint : Point; endPoint : Point; color : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)

let red = { r = 255uy; g = 0uy; b = 0uy}
let blue = { r = 0uy; g = 0uy; b = 255uy}
let green = { r = 0uy; g = 255uy; b = 255uy}

let randomColor() = { r = uint8(chaos.Next 256);g = uint8(chaos.Next 256);b = uint8(chaos.Next 256) }

//TODO 1 Do the Sierpinski triangle


type LogoCommand =
    | DrawForward of float 
    | MoveForward of float
    | Turn of float
    
   
type LTurtle = 
    { angle : float
      x : float
      y : float 
      c : Color}

/// interprets a logo program and produces a line segment list to render
let processTurtle turtle program =
    let rec phono output turtle = function
        | [] -> output
        | DrawForward d :: t -> 
            let rads = turtle.angle * (System.Math.PI / 180.0)
            let x = turtle.x + d * cos rads
            let y = turtle.y + d * sin rads
            let newTurtle = {turtle with x = x; y= y }
            let seg = 
                {   startPoint = {x = int turtle.x; y = int turtle.y}
                    endPoint = {x = int x; y = int y}
                    color = newTurtle.c }
            phono (seg::output) newTurtle t
            
        | MoveForward d :: t -> 
            let rads = turtle.angle * (System.Math.PI / 180.0)
            let x = turtle.x + d * cos rads
            let y = turtle.y + d * sin rads
            let newTurtle = {turtle with x = x; y= y }
            phono output newTurtle t

        | Turn delta :: t -> 
            let d = turtle.angle + delta
            let d =
                // warp around logic
                if delta > 0.0 && d > 360.0 then d - 360.0
                elif delta < 0.0 && d < 0.0 then 360.0 + d
                else d
            phono output {turtle with angle = d} t 
                
    List.rev(phono [] turtle program)

    
// TODO 2.1:  write a function that converts an initial string to productions like this
// Sierpinski  
// Start = "A"   
// Productions 
//      'A' -> "+B-A-B+" 
//      'B' -> "-A+B+A-" 
    
let processLsystem iterations currentString =
    ""
    
// TODO 2.2: Convert the string into turtle commands
let convertToTurtle comands =
    [Turn 50.0]
