module Game
    open Board

    type State =
        | ChooseMove
        | MoveSelected
        | GameOver of Team

    type Turn =
        | First
        | Second

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
        let mutable turn = Turn.First
                         
        member this.Board
            with get() = board

        member this.CellsX 
            with get() = cellsX

        member this.CellsY 
            with get() = cellsY

        member this.Turn
            with get() = turn
        
        member this.State
            with get() = state
        
        member this.MoveType
            with get() = moveType

        member this.MoveAllowed (board: BoardElement[,]) (moveType: MoveType) = 
            builder.MoveAllowed board moveType

        member this.SelectMove (move:MoveType) =
            moveType<-move
            state<-State.MoveSelected        

        member this.PerformMove (x,y) = 
            if x<0 || x>cellsX || y<(-1) || y>cellsY+1 then ()
            else
                if (state=State.MoveSelected && builder.MoveAllowedToPosition board moveType (x,y)) then
                    let el = if moveType=MoveType.Ball then BoardElement.Ball else BoardElement.Player
                    let gs,newBoard = builder.Move board moveType (x,y)
                    match gs with 
                    | GameState.On ->
                        state<-State.ChooseMove
                        turn<-if turn=Turn.First then Turn.Second else Turn.First
                        board<-newBoard
                    | GameState.Goal team ->
                        state<-State.GameOver team
                                   
        member this.Reset () =
            state<-State.ChooseMove
            turn<-Turn.First
            board<-initBoard()
