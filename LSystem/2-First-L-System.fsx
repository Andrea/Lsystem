#if INTERACTIVE
#else
module Domain
#endif

type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; } 

type LineSegment = {startPoint : Point; endPoint : Point; color : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)

let red = { r = 255uy; g = 0uy; b = 0uy}
let blue = { r = 0uy; g = 0uy; b = 255uy}
let green = { r = 0uy; g = 255uy; b = 255uy}

let randomColor() = { r = uint8(chaos.Next 256);g = uint8(chaos.Next 256);b = uint8(chaos.Next 256) }

// A basic LOGO system
type LogoCommand =
    | DrawForward of float 
    | Turn of float    
   
type LTurtle = 
    { angle : float
      x : float
      y : float 
      c : Color}

// a default turtle location
let turtle = { x = 0.0; y = 0.0; angle = 0.0; c = red }

/// interprets a logo program and produces a line segment list to render
let processTurtle turtle program =
    let rec aux output turtle = function
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
            aux (seg::output) newTurtle t
            
        | Turn delta :: t -> 
            let d = turtle.angle + delta
            let d =
                // warp around logic
                if delta > 0.0 && d > 360.0 then d - 360.0
                elif delta < 0.0 && d < 0.0 then 360.0 + d
                else d
            aux output {turtle with angle = d} t 
                
    List.rev(aux [] turtle program)
    
// TODO 2.1:  write a function that converts an initial string 
// using the following productions:
// Sierpinski  
// Start = "A"   
// Productions 
//      'A' -> "+B-A-B+" 
//      'B' -> "-A+B+A-" 
    
let processLsystem maxIterations =
    let rec proc iter (current: string) =
      if iter = maxIterations then current
      else
          // the stringbuilder is a fast mutable type
          let sb = System.Text.StringBuilder()  
          let append (s:string) = sb.Append s |> ignore        
          // todo: go through each character and replace with the rules above.
          // or use the same character if unrecognised
          // you can use the append function to add strings to the builder
          
                 
          // todo: call the next iteration recursively with the results
          (sb.ToString())
    proc 0 "A"


// Verify it works with this :D
// you can execute these test functions and they should return true!
let test1  = processLsystem 1 = "+B-A-B+"  
let test2  = processLsystem 2 = "+-A+B+A--+B-A-B+--A+B+A-+"  


// TODO 2.2: Convert the string into turtle commands
// A and B do the same thing - draw forward 
// + turns 60 degrees
// - turns -60 degrees

let defaultLength = 20.0
let defaultAngle = 60.0

let convertToTurtle (lSystemString: string) =
    // The values below are just so that this file compiles, complete a valid implemntation of this function
    [DrawForward(defaultLength);Turn(defaultAngle)]
 
// Verify that the function convertToTurtle works as expected
let test3 = 
    let commands = processLsystem 1 |>convertToTurtle        
    commands =
        [Turn(defaultAngle)          // +
         DrawForward(defaultLength)  // B
         Turn(-defaultAngle)         // -
         DrawForward(defaultLength)  // A
         Turn(-defaultAngle)         // -
         DrawForward(defaultLength)  // B   
         Turn(defaultAngle)]         // +
         

//TODO 2.3 
// From your renderer script (SDL.fsx or SVG.fsx) you can now call 
// processLsystem n |> convertToTurtle |> processTurtle turtle
// this will give you a list of line segments to be rendered
// using either the replace function (sdl) or toSvg |> save (svg)
// eg
//  SDL
//  processLsystem 1 
//  |> convertToTurtle        
//  |> processTurtle turtle
//
