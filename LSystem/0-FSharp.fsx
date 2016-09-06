
// How to define a value
let myNumber = 4
// select the code and press Alt+Enter to run it, it should show something like
// > 
// val myNumber : int = 4
// meaninng that the compiler set a value called myNumer to the integer 4, the 
//compiler infered the type because of the value we gave it.



// Let's define a function now
let add x y = x + y
// run that with Alt+Enter, you can also try it with the call below

add 3 4 
// Because we are using + with the x and y
// then the compiler infered that x and y are ints and hence the result of the funciton 
// will also be an int (you can change this )


// How we define a type in F# , this would be the equivalent of a POJO (Java) or POCO (C#)
type Point = { x : int; y : int }

//  Sometimes we need to define ADTs, these are  some types that allow you to 
//describe something with a finite set of values 

type BodyParts =  
    | Hand
    | Foot
    | Torso
    | Leg

// when you have an ADT (in F# we call them discriminated unions)

let find part = 
    match part with
    | Hand -> "woohoo you found a hand!"
    | Foot -> "Nice you found a foot "
    | _ -> "All the other parts"

//recursive functions. These are functions that call themselves

let rec factorial n = 
    match n with
    | 0 -> 1
    | x -> factorial(n-1) * x

factorial 4


