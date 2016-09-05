                
#r @"..\Lib\SDL2FS.dll"

// TODO: We will be changing this to load the other 
// fsx files as the workshop progresses.
#load "3-Generalized-L-system.fsx" 


open System
open Domain
open SDL.Geometry
open SDL.Pixel
open SDL.Render
open SDL.Surface

let windowWidth = 1280<SDL.px>
let windowHeight = 1024<SDL.px>
let fps = 5.0                                                                                                              
let delayTime = uint32(1000.0 / fps)

type GraphicsMessage =
    | Add of LineSegment list
    | Replace of LineSegment list

type state = 
    { system : SDL.Init.System; 
      window : SDL.Window.Window; 
      renderer : SDL.Render.Renderer; 
      lines : LineSegment list }

let agent = MailboxProcessor<GraphicsMessage>.Start(fun inbox -> 
    let system = new SDL.Init.System(SDL.Init.Init.Everything)
    let window = SDL.Window.create "SDL2FS" (100<SDL.px>, 100<SDL.px>) (windowWidth,windowHeight) (SDL.Window.Flags.Shown)
    let mainRenderer = SDL.Render.create window -1 SDL.Render.Flags.Accelerated  
    let state = {system = system; window=window; renderer=mainRenderer; lines = []}
    let toSDLPoint(p:Domain.Point) = { X = p.x*1<SDL.px>; Y = p.y*1<SDL.px> } : SDL.Geometry.Point
    let rec coreLoop state = async {
        match SDL.Event.poll() with
        | Some v when not v.isQuitEvent ->
            // handle events
            return! coreLoop state
        | Some v -> // quit
            state.renderer.Destroy()
            state.window.Destroy()
            state.window.Destroy()
        | _ ->
            let! msg = inbox.TryReceive 0
            match msg with
            | Some(Add lines) -> return! coreLoop {state with lines = (lines @ state.lines) }
            | Some(Replace lines) -> return! coreLoop {state with lines = lines }
            | None -> 
                let frameStart = SDL.Utility.getTicks()
                let render() = 
                    state.renderer
                    |> SDL.Render.setDrawColor(40uy,40uy,40uy,0uy)
                    |> ignore
            
                    state.renderer
                    |> SDL.Render.clear
                    |> ignore

                    for ls in state.lines do
                        state.renderer 
                        |> SDL.Render.setDrawColor(ls.color.r,ls.color.g,ls.color.b,0uy)
                        |> ignore
                
                        state.renderer
                        |> SDL.Render.drawLine(toSDLPoint ls.startPoint,toSDLPoint ls.endPoint)   
                        |> ignore

                    state.renderer
                    |> SDL.Render.present
                    |> ignore

                let delay() = 
                    let frameTime = SDL.Utility.getTicks() - frameStart
                    if frameTime < delayTime then
                        SDL.Utility.delay(delayTime-frameTime)
        
                render()
                delay()
            
                return! coreLoop state
                
    }
    coreLoop state
)

// helpers for clearing the screen, and adding lines to be rendered
let clear() = agent.Post <| Replace []
let replace lines = agent.Post <| Replace lines
let add lines = agent.Post <| Add lines


type LGen =
    | ForwardA
    | ForwardB
    | ForwardC
    | SkipD
    | SkipE
    | SkipF
    | PlaceholderG
    | PlaceholderH
    | PlaceholderI
    | TurnRight
    | TurnLeft
    | ChangeColour of int
    | Branch of LGen list
    
    with override x.ToString() = 
            match x with
            | ForwardA -> "A"
            | ForwardB -> "B"
            | ForwardC -> "C"
            | SkipD    -> "D"
            | SkipE     -> "E"
            | SkipF     -> "F"
            | PlaceholderG -> "G"
            | PlaceholderH  -> "H"
            | PlaceholderI -> "I"
            | TurnRight -> "+"  
            | TurnLeft -> "+"
            | ChangeColour i -> string i
            | Branch inner -> "[" + (inner |> List.map(fun i -> i.ToString()) |> String.concat "") + "]"

                
let gen () =
    let rec genRewrite n =
        let rec aux c data =
            if c = n then data
            else
                match chaos.Next(1,14) with
                | 1 -> aux (c+1) (ForwardA :: data)
                | 2 -> aux (c+1) (ForwardB :: data)
                | 3 -> aux (c+1) (ForwardC :: data)
                | 4 -> aux (c+1) (SkipD :: data)
                | 5 -> aux (c+1) (SkipE :: data)
                | 6 -> aux (c+1) (SkipF :: data)
                | 7 -> aux (c+1) (PlaceholderG :: data)
                | 8 -> aux (c+1) (PlaceholderH :: data)
                | 9 -> aux (c+1) (PlaceholderI :: data)
                | 10 ->aux (c+1) ( TurnRight :: data)
                | 11 ->aux (c+1) ( TurnLeft :: data)
                | 12 ->aux (c+1) ( ChangeColour(chaos.Next(0,10)) :: data)
                | 13 ->aux (c+1) ( Branch(genRewrite(chaos.Next(1,6)))  :: data)
        aux 0 []
    [for x in 0..chaos.Next(1,10) -> 
        let r = genRewrite(chaos.Next(1,10))
        match chaos.Next(1,10) with
        | 1 -> ForwardA, r
        | 2 -> ForwardB, r
        | 3 -> ForwardC, r
        | 4 -> SkipD, r
        | 5 -> SkipE, r
        | 6 -> SkipF, r
        | 7 -> PlaceholderG, r
        | 8 -> PlaceholderH, r
        | 9 -> PlaceholderI, r
                 ]
    |> List.map(fun (k,v) -> 
        printfn "%A -> %A" k v
        (char <| k.ToString(),v))
    |> Map.ofList

let randomLsystem() =
    let defaultLength = 5.0 + chaos.NextDouble() * 20.0
    let defaultAngle = chaos.NextDouble() * 90.0
    let things = gen()
    let co = [for x in 0..9 -> x, randomColor()] |> Map.ofList
    { Axiom = "ABCDEFGHI"
      Productions = 
        fun c -> 
            match things.TryFind c with
            | Some v -> v.ToString()
            | None -> string c
      Actions = 
        fun max c -> 
            match c with 
            | 'A' -> Some <| [DrawForward(defaultLength)]
            | 'B' -> Some <| [DrawForward(defaultLength)]
            | 'C' -> Some <| [DrawForward(defaultLength)]
            | 'D' -> Some <| [MoveForward(defaultLength)]
            | 'E' -> Some <| [MoveForward(defaultLength)]
            | 'F' -> Some <| [MoveForward(defaultLength)]
            | 'G' -> None
            | 'H' -> None
            | 'I' -> None
            | '+' -> Some <| [Turn(defaultAngle)]
            | '-' -> Some <| [Turn(-defaultAngle)]
            | '0' -> Some <| [ChangeColor(co.[0])]
            | '1' -> Some <| [ChangeColor(co.[1])]
            | '2' -> Some <| [ChangeColor(co.[2])]
            | '3' -> Some <| [ChangeColor(co.[3])]
            | '4' -> Some <| [ChangeColor(co.[4])]
            | '5' -> Some <| [ChangeColor(co.[5])]
            | '6' -> Some <| [ChangeColor(co.[6])]
            | '7' -> Some <| [ChangeColor(co.[7])]
            | '8' -> Some <| [ChangeColor(co.[8])]
            | '9' -> Some <| [ChangeColor(co.[9])]

            | _ -> None // todo
    
    }
//PlaceholderG -> [ForwardC; TurnRight; SkipD; SkipD]
//SkipD -> [Branch [ChangeColour 2]; ChangeColour 6; Branch [PlaceholderI; SkipF]; PlaceholderI; ForwardC; PlaceholderH]
//PlaceholderH -> [SkipF; ForwardB; TurnRight; ForwardB; ForwardB; PlaceholderI; PlaceholderI; PlaceholderI; TurnLeft]
//PlaceholderI -> [Branch [TurnRight; SkipF; TurnRight; ForwardA; ForwardC]; SkipF; ChangeColour 1; TurnRight]
//ForwardC -> [TurnLeft; PlaceholderG; TurnLeft; ForwardA]
//SkipE -> [ForwardB; PlaceholderI; SkipF; SkipF; PlaceholderI; SkipF; TurnRight]
//PlaceholderH -> [ChangeColour 6; ForwardC; ForwardC; ForwardA; TurnLeft; PlaceholderG]
//ForwardB -> [PlaceholderH; PlaceholderH; ForwardC; TurnRight; SkipD; ForwardA; SkipD]
//PlaceholderH -> [TurnLeft; TurnRight;
// Branch [PlaceholderG; ForwardB; ForwardA; ForwardB; TurnRight]; PlaceholderH;
//SkipD; PlaceholderI; SkipD]
//PlaceholderG -> [PlaceholderG; ChangeColour 3; PlaceholderH; PlaceholderH]

randomLsystem()
|> processLsystem 10
|> processTurtle turtle
|> replace

