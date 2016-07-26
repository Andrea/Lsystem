
type Point = { x : int; y : int }

type Color = { r:byte; g:byte; b:byte; } 

type LineSegment = {startPoint : Point; endPoint : Point; color : Color }    

let chaos = System.Random(System.DateTime.Now.Millisecond)

let red = { r = 255uy; g = 0uy; b = 0uy}

type LogoCommand =
    | Forward of float * Color option
    | Turn of float
   
type LTurtle = 
    { angle : float
      x : float
      y : float } 

/// interprets a logo program and produces a line segment list to render
let processTurtle turtle program =
    let rec phono output turtle = function
        | [] -> output
        | Forward(d,color) :: t -> 
            let rads = turtle.angle * (System.Math.PI / 180.0)
            let x = turtle.x + d * cos rads
            let y = turtle.y + d * sin rads
            let newTurtle = {turtle with x = x; y= y }
            match color with
            | Some c -> 
                let seg = 
                    { startPoint = {x = int turtle.x; y = int turtle.y}
                      endPoint = {x = int x; y = int y}
                      color = c }
                phono (seg::output) newTurtle t
            | None -> phono output newTurtle t
        | Turn delta :: t -> 
            let d = turtle.angle + delta
            let d =
                // warp around logic
                if delta > 0.0 && d > 360.0 then d - 360.0
                elif delta < 0.0 && d < 0.0 then d + 360.0
                else d
            phono output {turtle with angle = d} t

    List.rev(phono [] turtle program)


let lsystem (axiom:string) iterations =
    let rec juan (current:string) iteration =
        if iteration = iterations then current
        else
            let sb = System.Text.StringBuilder()
            for x in current.ToCharArray() do
                if x = 'A' then sb.Append("ABA") |> ignore
                elif x = 'B' then sb.Append("BBB") |> ignore
                else sb |> ignore
            juan (sb.ToString()) (iteration+1)
    juan axiom 0
    