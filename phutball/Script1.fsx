open System

type BoardElement = 
    | Empty
    | Player
    | Ball

type MoveType = 
    | Player
    | Ball

type Position = Position of int*int

type BallDirection = 
    | NorthWest
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West

let printElement (v: BoardElement) = 
    match v with
    | BoardElement.Empty -> "."
    | BoardElement.Player -> "x"
    | BoardElement.Ball -> "o"

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

let findBall (board: BoardElement[,]) =
    board
    |> Array2D.mapi (fun i j el -> i,j,el)
    |> Seq.cast<int*int*BoardElement>
    |> Seq.find (fun (_,_,el) -> el=BoardElement.Ball)
    |> fun (i,j,_) -> (i,j)

let availablePositions (board: BoardElement[,]) (moveType: MoveType) =
    match moveType with
    | MoveType.Player ->
        board
        |> Array2D.mapi (fun i j el -> if el=BoardElement.Empty then (i,j) else (-1,j))
        |> Seq.cast<int*int>
        |> Seq.filter (fun (i,j) -> i>=0)
    | MoveType.Ball ->
        let n = board |> Array2D.length1
        let m = board |> Array2D.length2
        let i,j = findBall board        
        match i,j with
        | 0,0 -> 
            [BallDirection.East; BallDirection.SouthEast; BallDirection.South]
        | 0,m-1 ->
            [BallDirection.South; BallDirection.SouthWest; BallDirection.West]
        | 0,_ -> 
            [BallDirection.East; BallDirection.SouthEast; BallDirection.South; BallDirection.SouthWest; BallDirection.West]
        | n-1,0 ->
        | n-1,m-1 ->
        | n-1, _ ->
        | _, 0 ->
        | _, m-1 ->
        | _ ->

let move (board: BoardElement[,]) (moveType: MoveType) (position: Position) =
    let (Position (x,y)) = position
    match board.[x,y] with
    | BoardElement.Empty -> failwith "Position already occupied"
    | _ ->
        match moveType with
        | MoveType.Player -> 
            board.[x,y]<-BoardElement.Player
        | MoveType.Ball -> 
            board.[x,y]<-BoardElement.Ball
        board
                 

let board = initialize (19,15)
print board
