#r @"..\Lib\SDL2FS.dll"
#load "Domain.fsx"
open System
open Domain
open SDL.Geometry
open SDL.Pixel
open SDL.Render
open SDL.Surface

let windowWidth = 800<SDL.px>
let windowHeight = 600<SDL.px>
let fps = 60.0
let delayTime = uint32(1000.0 / fps)
let chaos = System.Random(System.DateTime.Now.Millisecond)

let lines = ResizeArray<Domain.LineSegment>()

let toSDLPoint(p:Domain.Point) = { X = p.x*1<SDL.px>; Y = p.y*1<SDL.px> } : SDL.Geometry.Point

let createLogoRenderer () = 
    use system = new SDL.Init.System(SDL.Init.Init.Everything)
    use window = SDL.Window.create "SDL2FS" (100<SDL.px>, 200<SDL.px>) (windowHeight,windowHeight) (SDL.Window.Flags.Shown)
    use mainRenderer = SDL.Render.create window -1 SDL.Render.Flags.Accelerated  

    let rec coreLoop () : unit =
        let frameStart = SDL.Utility.getTicks()

        let render() = 
            mainRenderer 
            |> SDL.Render.setDrawColor(0uy,0uy,0uy,0uy)
            |> ignore
            
            mainRenderer
            |> SDL.Render.clear
            |> ignore

            for ls in lines do
                mainRenderer 
                |> SDL.Render.setDrawColor(ls.color.r,ls.color.g,ls.color.b,0uy)
                |> ignore
                
                mainRenderer
                |> SDL.Render.drawLine(toSDLPoint ls.startPoint,toSDLPoint ls.endPoint)   
                |> ignore

            mainRenderer
            |> SDL.Render.present
            |> ignore

        let delay() = 
            let frameTime = SDL.Utility.getTicks() - frameStart
            if frameTime < delayTime then
                SDL.Utility.delay(delayTime-frameTime)
        
        match SDL.Event.poll() with
        | Some v when not v.isQuitEvent ->
            // handle events
            coreLoop()
        | Some v -> // quit
            ()
        | _ -> 
            render()
            delay()
            coreLoop()

    coreLoop()

let createWindow = async {
    return createLogoRenderer()
}

let randomShit n =
    lines.Clear()
    let rby() = uint8 <| chaos.Next 256
    for x in 1..n do
        let c = { r=rby(); g=rby(); b=rby(); } : Domain.Color
        let rpy() = 
            {x = chaos.Next(int windowWidth); 
             y = chaos.Next(int windowWidth)} : Domain.Point
        lines.Add({startPoint=rpy(); endPoint=rpy(); color = c})
    
let drawSegments segs =
    lines.Clear()
    for seg in segs do 
        lines.Add seg

createWindow |> Async.Start

let testProgram = 
    [for x in 1..8 do
         yield Forward(10.0,Some red)
         yield Forward(10.0,None)
         yield Turn 45.0 ]

let executeProgram program =
    let t = {angle = 0.0; x = 100.0; y = 100.0}
    drawSegments (processTurtle t program)

randomShit 50

executeProgram testProgram
