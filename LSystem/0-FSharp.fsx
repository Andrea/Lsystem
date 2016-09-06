
// This is a whitespace sensitive Language

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


// conditionals can be used without parens but you do need and else 
let x = if true then 1 else 2


//Creating lists
// use brackets, not the semi colon ;
let myList = [1;2;3;4]

let someString = "asdasd"
let anotherList = [someString]


// calling some functions using the |> operator 

let aList = [1..4]
aList
|> List.filter( fun x -> x > 1)
|> List.sum


(* Some stuff that might be useful during this workshop *)


// Iterate over a string and 
let myString = "Monkeys"
let myResult = for x in myString do printf "%c and cat " x


// convert from char to string

let myChar = 'v'
let aString : string  = (string myChar)

