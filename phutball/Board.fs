module Board
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

    let boardDimensions (board: BoardElement[,]) =
        board |> Array2D.length1, board |> Array2D.length2

    let initializeBoard (n: int, m: int) = 
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
            if x>=0 && y>=0 then
                match direction with
                | BallDirection.NorthWest ->
                    if board.[x-1,y-1]=BoardElement.Player then
                        avance direction (x-1) (y-1)
                    else x,y
                | BallDirection.North ->
                    if board.[x,y-1]=BoardElement.Player then
                        avance direction x (y-1)
                    else x,y
                | BallDirection.NorthEast ->
                    if board.[x+1,y-1]=BoardElement.Player then
                        avance direction (x+1) (y-1)
                    else x,y
                | BallDirection.East ->
                    if board.[x+1,y]=BoardElement.Player then
                        avance direction (x+1) y
                    else x,y
                | BallDirection.SouthEast ->
                    if board.[x+1,y+1]=BoardElement.Player then
                        avance direction (x+1) (y+1)
                    else x,y
                | BallDirection.South ->
                    if board.[x,y+1]=BoardElement.Player then
                        avance direction x (y+1)
                    else x,y
                | BallDirection.SouthWest ->
                    if board.[x-1,y+1]=BoardElement.Player then
                        avance direction (x-1) (y+1)
                    else x,y
                | BallDirection.West ->
                    if board.[x-1,y]=BoardElement.Player then
                        avance direction (x-1) y
                    else x,y
            else
                x,y

        [BallDirection.NorthWest
         BallDirection.North
         BallDirection.NorthEast
         BallDirection.East
         BallDirection.SouthEast
         BallDirection.South
         BallDirection.SouthWest
         BallDirection.West]
         |> List.map (fun direction -> avance direction i j)
         |> List.filter (fun (x,y) -> x<>i || y<>j)

    let possiblePlayerPositions (board: BoardElement[,]) =
        let empty = board
                    |> Array2D.mapi (fun i j el -> if el=BoardElement.Empty then (i,j) else (-1,j))
                    |> Seq.cast<int*int>
                    |> Seq.filter (fun (i,j) -> i>=0)
                    |> List.ofSeq
        // can't allow the last empty position to be occupied by a player
        if empty.Length>1 then empty else []

    let moveAllowed (board: BoardElement[,]) (moveType: MoveType) (x,y) = 
        match moveType with
        | MoveType.Player ->
            board.[x,y]=BoardElement.Empty
        | MoveType.Ball ->
            let positions = possibleBallPositions board
            List.exists (fun (i,j) -> i=x&&j=y) positions

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
        | BoardElement.Ball | BoardElement.Player -> failwith "Position already occupied"
        | BoardElement.Empty ->
            match moveType with
            | MoveType.Player -> 
                board.[x,y]<-BoardElement.Player
                GameState.On,board
            | MoveType.Ball -> 
                moveBall board (x,y)
       
