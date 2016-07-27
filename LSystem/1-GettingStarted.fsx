module Domain

type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; } 

type LineSegment = {startPoint : Point; endPoint : Point; color : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)

let red = { r = 255uy; g = 0uy; b = 0uy}
let blue = { r = 0uy; g = 0uy; b = 255uy}
let green = { r = 0uy; g = 255uy; b = 255uy}

let randomColor() = { r = uint8(chaos.Next 256);g = uint8(chaos.Next 256);b = uint8(chaos.Next 256) }

//TODO 0 Does this thing Draws randomShit?
let randomShit n =
    let rby() = uint8 <| chaos.Next 256
    [for x in 1..n do
        let c = { r=rby(); g=rby(); b=rby() } : Color
        let rpy() = 
            {x = chaos.Next(int 400); 
             y = chaos.Next(int 400)} : Point
        yield {startPoint=rpy(); endPoint=rpy(); color = c}]