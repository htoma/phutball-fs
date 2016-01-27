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

    type Team =
        | Up
        | Down

    type GameState =
        | On
        | Goal of Team

    let boardDimensions (board: BoardElement[,]) =
        board |> Array2D.length1, board |> Array2D.length2

    let initializeBoard (n: int, m: int) = 
        match n,m with
        | x,y when x>1 && y>1 && x%2>0 && y%2>0 -> 
              let board = Array2D.init n m (fun _ _ -> BoardElement.Empty)
              board.[(n-1)/2,(m-1)/2]<- BoardElement.Ball
              board
        | _ -> failwith "Board dimensions should not be even"

    let findBall (board: BoardElement[,]) =
        board
        |> Array2D.mapi (fun i j el -> i,j,el)
        |> Seq.cast<int*int*BoardElement>
        |> Seq.find (fun (_,_,el) -> el=BoardElement.Ball)
        |> fun (i,j,_) -> (i,j)

    let possibleBallPositions (board: BoardElement[,]) =
        let i,j = findBall board
        let m,n = boardDimensions board
        let rec avance (direction: BallDirection) x y start =
            //some positions may result in goals
            if x<0 || x=m || y=(-1) || y=n then None
            else
                match direction with
                | BallDirection.NorthWest ->
                    if x=0 then None
                    elif y=0 then if start then None else Some(x-1,y-1) //Goal!
                    elif board.[x-1,y-1]=BoardElement.Player then
                        avance direction (x-1) (y-1) false
                    else
                        if start then None else Some(x-1,y-1)                
                | BallDirection.North ->
                    if y=0 then Some(x,y-1) //Goal!
                    elif board.[x,y-1]=BoardElement.Player then
                        avance direction x (y-1) false
                    else
                        if start then None else Some(x,y-1)
                | BallDirection.NorthEast ->
                    if x=m-1 then None 
                    elif y=0 then if start then None else Some(x+1,y-1) //Goal!
                    elif board.[x+1,y-1]=BoardElement.Player then
                        avance direction (x+1) (y-1) false
                    else
                        if start then None else Some(x+1,y-1)
                | BallDirection.East ->
                    if x=m-1 then None
                    elif board.[x+1,y]=BoardElement.Player then
                        avance direction (x+1) y false
                    else
                        if start then None else Some(x+1,y)
                | BallDirection.SouthEast ->
                    if x=m-1 then None
                    elif y=n-1 then if start then None else Some (x+1,y+1) //Goal!
                    elif board.[x+1,y+1]=BoardElement.Player then
                        avance direction (x+1) (y+1) false
                    else
                        if start then None else Some(x+1,y+1)
                | BallDirection.South ->
                    if y=n-1 then Some(x,y+1) //Goal!
                    elif board.[x,y+1]=BoardElement.Player then
                        avance direction x (y+1) false
                    else
                        if start then None else Some(x,y+1)
                | BallDirection.SouthWest ->
                    if x=0 then None
                    elif y=n-1 then if start then None else Some(x-1,y+1)
                    elif board.[x-1,y+1]=BoardElement.Player then
                        avance direction (x-1) (y+1) false
                    else
                        if start then None else Some(x-1,y+1)
                | BallDirection.West ->
                    if x=0 then None
                    elif board.[x-1,y]=BoardElement.Player then
                        avance direction (x-1) y false
                    else
                        if start then None else Some(x-1,y)
        
        [BallDirection.NorthWest
         BallDirection.North
         BallDirection.NorthEast
         BallDirection.East
         BallDirection.SouthEast
         BallDirection.South
         BallDirection.SouthWest
         BallDirection.West]
         |> List.map (fun direction -> avance direction i j true)
         |> List.filter Option.isSome

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
            List.exists (fun v -> match v with
                                  | Some (i,j) -> i=x && j=y
                                  | None -> false) positions
    
    let teamThatScored y =
        GameState.Goal (if y=(-1) then Team.Up else Team.Down) 

    let moveBall (board: BoardElement[,]) (x,y) = 
        let a,b = findBall board
        let m,n = boardDimensions board
        if a=x && b=y then GameState.On,board
        else
            if x>=0 && x<m && (y=(-1) || y=n) then
                teamThatScored y,board
            else
                let direction =
                    if x=a then
                        if y>b then BallDirection.North else BallDirection.South
                    elif y=b then
                        if x<a then BallDirection.West else BallDirection.East
                    elif x<a then
                        if y<b then BallDirection.SouthWest else BallDirection.NorthWest
                    else
                        if y<b then BallDirection.SouthEast else BallDirection.NorthEast
                
                //clean the players jumped
                match direction with
                 | BallDirection.NorthWest ->
                    for i in [x+1..a-1] do
                        board.[i,y-(a-i)]<-BoardElement.Empty
                 | BallDirection.North ->
                    for j in [b+1..y-1] do
                        board.[x,j]<-BoardElement.Empty
                 | BallDirection.NorthEast ->
                    for i in [a+1..x-1] do
                        board.[i,y-(x-i)]<-BoardElement.Empty
                 | BallDirection.East ->
                    for i in [a+1..x-1] do
                        board.[i,y]<-BoardElement.Empty
                 | BallDirection.SouthEast ->
                    for i in [a+1..x-1] do
                        board.[i,y+(x-i)]<-BoardElement.Empty
                 | BallDirection.South ->
                    for j in [y+1..b-1] do
                        board.[x,j]<-BoardElement.Empty
                 | BallDirection.SouthWest ->
                    for i in [x+1..a-1] do
                        board.[i,y+(a-i)]<-BoardElement.Empty
                 | BallDirection.West ->
                    for i in [x+1..a-1] do
                        board.[i,y]<-BoardElement.Empty 
                
                //move the ball
                board.[a,b]<-BoardElement.Empty
                board.[x,y]<-BoardElement.Ball
                    
                GameState.On,board
        
    let move (board: BoardElement[,]) (moveType: MoveType) (x,y) =
        let m,n = boardDimensions board
        if y=(-1) || y=n then
            teamThatScored y,board
        else
            match board.[x,y] with
            | BoardElement.Ball | BoardElement.Player -> failwith "Position already occupied"
            | BoardElement.Empty ->
                match moveType with
                | MoveType.Player -> 
                    board.[x,y]<-BoardElement.Player
                    GameState.On,board
                | MoveType.Ball -> 
                    printfn "Moving ball at %i and %i" x y
                    moveBall board (x,y)
       
