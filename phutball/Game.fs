module Game
    open Board
   
    type State =
        | ChooseMove
        | MoveSelected
        | GameOver of Team

    type Game () = 
        //constants
        let cellsX = 14    
        let cellsY = 18

        let builder = BoardBuilder()
  
        let initBoard () = 
            builder.InitializeBoard (cellsX+1,cellsY+1)

        let mutable board = initBoard()
        let mutable state = State.ChooseMove
        let mutable moveType = MoveType.Player
                         
        member this.Board
            with get() = board

        member this.CellsX 
            with get() = cellsX

        member this.CellsY 
            with get() = cellsY   

        member this.ChooseMove (move:MoveType) =
            moveType<-move
            state<-State.MoveSelected        
            
        member this.MoveAllowed (board: BoardElement[,]) (moveType: MoveType) = 
            builder.MoveAllowed board moveType

        member this.Move (board: BoardElement[,]) (moveType: MoveType) (x,y) =
            builder.Move board moveType (x,y)

        member this.Reset () =
            state<-State.ChooseMove
            board<-initBoard()

        member this.CellSelected x y = 
            if x<0 || x>cellsX || y<(-1) || y>cellsY+1 then ()
            else
                if (state=State.MoveSelected && builder.MoveAllowedToPosition board moveType (x,y)) then
                    let el = if moveType=MoveType.Ball then BoardElement.Ball else BoardElement.Player
                    let gs,newBoard = builder.Move board moveType (x,y)
                    match gs with 
                    | GameState.On ->
                        state<-State.ChooseMove
                        board<-newBoard
                    | GameState.Goal team ->
                        state<-State.GameOver team
               

   


