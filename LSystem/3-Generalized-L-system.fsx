module Domain

type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; } 

type LineSegment = {startPoint : Point; endPoint : Point; color : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)

let red = { r = 255uy; g = 0uy; b = 0uy}
let blue = { r = 0uy; g = 0uy; b = 255uy}
let green = { r = 0uy; g = 255uy; b = 255uy}

let randomColor() = { r = uint8(chaos.Next 256);g = uint8(chaos.Next 256);b = uint8(chaos.Next 256) }

type LogoCommand =
    | DrawForward of float 
    | MoveForward of float
    | ChangeColor of Color
    | Turn of float
       
type LTurtle = 
    { angle : float
      x : float
      y : float 
      c : Color} 

let turtle = { x = 400.0; y = 400.0; angle = 0.0; c = red }

/// interprets a logo program and produces a line segment list to render
let processTurtle turtle program =
    let rec aux output turtle = function
        | [] -> output
        | ChangeColor c :: t -> aux output {turtle with c = c} t
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
            
        | MoveForward d :: t -> 
            let rads = turtle.angle * (System.Math.PI / 180.0)
            let x = turtle.x + d * cos rads
            let y = turtle.y + d * sin rads
            let newTurtle = {turtle with x = x; y= y }
            aux output newTurtle t

        | Turn delta :: t -> 
            let d = turtle.angle + delta
            let d =
                // warp around logic
                if delta > 0.0 && d > 360.0 then d - 360.0
                elif delta < 0.0 && d < 0.0 then 360.0 + d
                else d
            aux output {turtle with angle = d} t
    List.rev(aux [] turtle program)

    
type LSystem = {
    Axiom : string
    Productions : char -> string
    Actions : int  -> char -> (LogoCommand list) option
}

let processLsystem max lsystem =
    // first we perform the l-system generation
    // fast imperative generator
    let rec gen (current:string) iteration =
        if iteration = max then current
        else
            let sb = System.Text.StringBuilder()
            for x in current do sb.Append(lsystem.Productions x) |> ignore
            gen (sb.ToString()) (iteration+1)
              
    let finish = gen lsystem.Axiom 0
    // now convert to turtle commands
    finish.ToCharArray() 
    |> List.ofArray 
    |> List.choose (lsystem.Actions max) 
    |> List.collect id





// TODO 3 :These are some L-systems... go nuts!!! 
//Change colours, change the productions, etc!!

let sierpinski width = {
    // https://en.wikipedia.org/wiki/Sierpinski_triangle
    Axiom = "A"
   
    Productions =
        function
        | 'A' -> "+B-A-B+" 
        | 'B' -> "-A+B+A-" 
        | c -> string c

    Actions = 
        fun max c -> 
            let length =   
                let width = float width
                if max = 0 then width 
                else width / (System.Math.Pow(2.0, float max))
     
            match c with
            | 'A' -> Some <| [DrawForward(length)]
            | 'B' -> Some <| [MoveForward(length)]
            | '+' -> Some <| [Turn 60.0]
            | '-' -> Some <| [Turn -60.0]
            | _ -> None
}



let koch = {
    //https://en.wikipedia.org/wiki/Koch_snowflake
    // warning! go easy on the amount of iterations on this one!
    Axiom = "F"
   
    Productions = 
        function
        | 'F' -> "F+F-F-F+F" 
        | c -> string c

    Actions = 
        fun max c -> 
            let length = 2.0
            match c with
            | 'F' -> Some <| [DrawForward(length)]
            | '+' -> Some <| [Turn 90.0]
            | '-' -> Some <| [Turn -90.0]
            | _ -> None
}


let cantor width = {
    // https://en.wikipedia.org/wiki/Cantor_set
    // to see this one in all it's glory, you will need to plot
    // each iteration on successive rows!
    Axiom = "A"
    Productions = function 'A' -> "ABA" | _ -> "BBB"
    Actions = 
        fun max c -> 
            let length =   
                let width = float width
                if max = 0 then width 
                else width / (System.Math.Pow(3.0, float max))
     
            match c with 
            | 'A' -> Some <| [DrawForward(length)]
            | _ -> Some <| [MoveForward(length)]
    
}

let dragColours = [for x in 0..20 -> x,randomColor()] |> dict

let dragon = {
    //https://en.wikipedia.org/wiki/Dragon_curve
    Axiom = "FX"
   
    Productions = 
        function
        | 'X' -> "X+YF+F" 
        | 'Y' -> "XX-FX-YY"
        | c -> string c

    Actions = 
        fun max c -> 
            let length = float <| chaos.Next(1,10)
            match c with
            | 'F' -> Some <| [DrawForward(length)]
            | '+' -> Some <| [Turn (float(chaos.Next(45,90)))]
            | '-' -> Some <| [Turn -90.0]
            | 'Z' -> Some <| [ChangeColor(randomColor())]
            | _ -> None

}