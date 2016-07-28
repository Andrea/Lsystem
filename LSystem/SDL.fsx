#r @"..\Lib\SDL2FS.dll"

// TODO: We will be changing this to load the other 
// fsx files as the workshop progresses.
#load "1-GettingStarted.fsx" 


open System
open Domain
open SDL.Geometry
open SDL.Pixel
open SDL.Render
open SDL.Surface

let windowWidth = 1280<SDL.px>
let windowHeight = 1024<SDL.px>
let fps = 60.0                                                                                                              
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
                    |> SDL.Render.setDrawColor(0uy,0uy,0uy,0uy)
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

// draw some crap to the screen
randomPOOP 20
|> replace
