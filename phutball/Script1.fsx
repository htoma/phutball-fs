open System

type BoardElement = 
    | Empty
    | Player
    | Ball

let printElement (v: BoardElement) = 
    match v with
    | Empty -> "."
    | Player -> "x"
    | Ball -> "o"

let print board = 
    let n = board |> Array2D.length1
    let m = board |> Array2D.length2
    for i in [0..n-1] do
        printf "|"
        for j in [0..m-1] do
            printf "%s" (board.[i,j] |> printElement)
        printfn "|"
    
let initialize (n: int, m: int) = 
    match n,m with
    | x,y when x>1 && y>1 && x%2>0 && y%2>0 -> 
          let board = Array2D.init n m (fun _ _ -> BoardElement.Empty)
          board.[(n-1)/2,(m-1)/2]<- BoardElement.Ball
          board
    | _ -> failwith "Board dimensions should be not even"

let board = initialize (19,15)
print board