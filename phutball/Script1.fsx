open System

type BoardElement = 
    | Empty
    | Player
    | Ball

type MoveType = 
    | Player
    | Ball

type BallDirection = 
    | NorthWest
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West

type GameState =
    | On
    | Over

let printElement (v: BoardElement) = 
    match v with
    | BoardElement.Empty -> "."
    | BoardElement.Player -> "x"
    | BoardElement.Ball -> "o"

let boardDimensions (board: BoardElement[,]) =
    board |> Array2D.length1, board |> Array2D.length2

let print board = 
    let n,m = boardDimensions board
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

let findBall (board: BoardElement[,]) =
    board
    |> Array2D.mapi (fun i j el -> i,j,el)
    |> Seq.cast<int*int*BoardElement>
    |> Seq.find (fun (_,_,el) -> el=BoardElement.Ball)
    |> fun (i,j,_) -> (i,j)

let possibleBallPositions (board: BoardElement[,]) =
    let i,j = findBall board
    let rec avance (direction: BallDirection) x y =
        if x>=0 && y>=0 && board.[x,y]=BoardElement.Player then
            match direction with
            | BallDirection.NorthWest ->
                avance direction (x-1) (y-1)
            | BallDirection.North ->
                avance direction (x-1) y
            | BallDirection.NorthEast ->
                avance direction (x-1) (y+1)
            | BallDirection.East ->
                avance direction x (y+1)
            | BallDirection.SouthEast ->
                avance direction (x+1) (y+1)
            | BallDirection.South ->
                avance direction (x+11) y
            | BallDirection.SouthWest ->
                avance direction (x+1) (y-1)
            | BallDirection.West ->
                avance direction x (y-1)
        else
            x,y
    [BallDirection.NorthWest
     BallDirection.North
     BallDirection.NorthEast
     BallDirection.East
     BallDirection.SouthEast
     BallDirection.South
     BallDirection.SouthWest
     BallDirection.South]
     |> List.map (fun direction -> avance direction i j)
     |> List.filter (fun (x,y) -> x<>i && y<>j)

let possiblePlayerPositions (board: BoardElement[,]) =
    let empty = board
                |> Array2D.mapi (fun i j el -> if el=BoardElement.Empty then (i,j) else (-1,j))
                |> Seq.cast<int*int>
                |> Seq.filter (fun (i,j) -> i>=0)
                |> List.ofSeq
    // can't allow the last empty position to be occupied by a player
    if empty.Length>1 then empty else []

let moveBall (board: BoardElement[,]) (x,y) = 
    let a,b = findBall board
    let n,m = boardDimensions board
    if a=x && b=y then GameState.On,board
    else
        if x<0 || x=n || y<0 || y=m then
            GameState.Over,board
        else
            let direction =
                if x=a then
                    if y>b then BallDirection.East else BallDirection.West
                elif y=b then
                    if x<a then BallDirection.North else BallDirection.South
                elif x<a then
                    if y<b then BallDirection.NorthWest else BallDirection.NorthEast
                else
                    if y<b then BallDirection.SouthWest else BallDirection.SouthEast
            match direction with
             | BallDirection.NorthWest ->
                for i in [x+1..a-1] do
                    board.[i,y-(a-i)]<-BoardElement.Empty
             | BallDirection.North ->
                for i in [a+1..x-1] do
                    board.[i,b]<-BoardElement.Empty
             | BallDirection.NorthEast ->
                for i in [a+1..x-1] do
                    board.[i,y-(a-i)]<-BoardElement.Empty
             | BallDirection.East ->
                for j in [b+1..y-1] do
                    board.[x,j]<-BoardElement.Empty
             | BallDirection.SouthEast ->
                for i in [a+1..x-1] do
                    board.[i,y+(a-i)]<-BoardElement.Empty
             | BallDirection.South ->
                for i in [x+1..a-1] do
                    board.[i,b]<-BoardElement.Empty
             | BallDirection.SouthWest ->
                for i in [a+1..x-1] do
                    board.[i,y-(a-i)]<-BoardElement.Empty
             | BallDirection.West ->
                for j in [y+1..b-1] do
                    board.[x,j]<-BoardElement.Empty 
                    
            GameState.On,board
        

let move (board: BoardElement[,]) (moveType: MoveType) (x,y) =
    match board.[x,y] with
    | BoardElement.Empty -> failwith "Position already occupied"
    | _ ->
        match moveType with
        | MoveType.Player -> 
            board.[x,y]<-BoardElement.Player
            GameState.On,board
        | MoveType.Ball -> 
            moveBall board (x,y)

let printPlayOption() =
    printfn "Select move"
    printfn "1 - Player"
    printfn "2 - Ball"

let printPositions positions =
    printfn "Available positions"
    positions
    |> List.map (fun (x,y) -> printf "(%i,%i)" x y)

let play (board: BoardElement[,]) = 
    let rec turn (state: GameState) (board: BoardElement[,]) = 
        if turn=GameState.Over then
            printfn "Game Over!"
        else
            print board
            printPlayOption()
            let key = Console.ReadKey()
            match key with
            | 1 ->
                let positions = possiblePlayerPositions board
                match positions with
                | [] -> 
                    printfn "There is no available position for placing a player"
                    turn GameState.On board
                | _ ->
                    printPositions positions
                    printfn "Select a position for your player: x,y"
                    let line = Console.ReadLine()
                    
    

let board = initialize (19,15)
print board
