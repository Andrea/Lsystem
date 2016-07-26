#r @"C:\repos\SDLFS\SDL2FS\bin\Debug\SDL2FS.dll"

open System

open SDL.Geometry
open SDL.Pixel
open SDL.Render
open SDL.Surface

let windowWidth = 800<SDL.px>
let windowHeight = 600<SDL.px>
let fps = 60.0
let delayTime = uint32(1000.0 / fps)
let chaos = System.Random(System.DateTime.Now.Millisecond)

type LineSegment = {startPoint : Point; endPoint : Point; colour : Color }    

let lines = ResizeArray<LineSegment>()

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
                |> SDL.Render.setDrawColor(ls.colour.Red,ls.colour.Green,ls.colour.Blue,ls.colour.Alpha)
                |> ignore
                
                mainRenderer
                |> SDL.Render.drawLine(ls.startPoint,ls.endPoint)   
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
        let c = { Red=rby(); Green=rby(); Blue=rby(); Alpha=0uy }
        let rpy() = 
            {X = chaos.Next(int windowWidth)*1<SDL.px>; 
             Y = chaos.Next(int windowWidth)*1<SDL.px>} : Point
        lines.Add({startPoint=rpy(); endPoint=rpy(); colour = c})
    

createWindow |> Async.Start

randomShit 50