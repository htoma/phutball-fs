#r @"C:\work\phutball\phutball\bin\Debug\phutball.exe"
open Board

open System.Windows.Forms 
open System.Drawing

type State =
    | ChooseMove
    | MoveSelected

type BoardForm() = 
    //constants
    let width = 370
    let height = 610
    let cellsX = 14    
    let cellsY = 18
    let offsetMarkersX = 20
    let offsetMarkersY = 20
    let offsetX = offsetMarkersX+20
    let offsetY = offsetMarkersY+20
    let offsetTextX = 15
    let offsetTextY = 12
    let cellSize = 20

    let mutable board = initializeBoard (cellsX+1,cellsY+1)
    let mutable state = State.ChooseMove
    let mutable moveType = MoveType.Player

    let drawButton (form:Form) (x,y) caption (sizeX, sizeY) enabled callback = 
        let button = new Button(Text=caption, Top=y, Left=x, 
                                BackColor=Color.Beige, Size=new Size(sizeX, sizeY), Enabled=enabled)
        button.Click.Add(callback)
        form.Controls.Add button

    let drawElement (g:Graphics) x y (color:Color) =
        use brush = new SolidBrush(color)
        g.FillEllipse(brush, new Rectangle(offsetX+x*cellSize-cellSize/2, offsetY+(cellsY-y+1)*cellSize-cellSize/2, cellSize, cellSize))

    let drawStone (g:Graphics) x y (el:BoardElement) =
        drawElement g x y (if el=BoardElement.Ball then Color.Coral else Color.Blue)

    let drawPossiblePositions (g:Graphics) (positions:(int*int) list) =
        positions
        |> List.iter (fun (i,j) -> drawElement g i j Color.AntiqueWhite)
            
    let rec drawBoard (form:Form) (board:BoardElement[,]) = 
        let chooseMove (move:MoveType) =
            fun _ ->
                moveType<-move
                state<-State.MoveSelected
                drawBoard form board
        let width = cellsX*cellSize
        let height = cellsY*cellSize
        let goalPosts = 2
        let heightWithPosts = height+2*cellSize

        use g = form.CreateGraphics()
        use font = new Font("Arial", 10.f)
        use textBrush = new SolidBrush(Color.Black)
        for i in [cellsY+2..-1..0] do
            g.DrawString(sprintf "%2i" i, font, textBrush, float32 offsetMarkersX, float32 (offsetMarkersY+offsetTextY+cellSize*(cellsY+2-i)))
            g.DrawString(sprintf "%2i" i, font, textBrush, float32 (offsetX+width), float32 (offsetMarkersY+offsetTextY+cellSize*(cellsY+2-i)))
        ["A";"B";"C";"D";"E";"F";"G";"H";"J";"K";"L";"M";"N";"O";"P"]
        |> List.iteri (fun i v -> 
            g.DrawString(v, font, textBrush, float32 (offsetMarkersX+offsetTextX+i*cellSize), float32 offsetMarkersY)
            g.DrawString(v, font, textBrush, float32 (offsetMarkersX+offsetTextX+i*cellSize), float32 (offsetY+heightWithPosts+5)))
    
        use brush = new SolidBrush(Color.Beige)
        g.FillRectangle(brush, offsetX, offsetY+cellSize, width, height)

        use goalBrush = new SolidBrush(Color.LawnGreen)
        g.FillRectangle(goalBrush, offsetX, offsetY, width, cellSize)
        g.FillRectangle(goalBrush, offsetX, offsetY+cellSize+height, width, cellSize)

        use pen = new Pen(Brushes.Black)
        for i in [0..cellsX] do
            g.DrawLine(pen, offsetX+i*cellSize, offsetY, offsetX+i*cellSize, offsetY+heightWithPosts)
        for i in [0..cellsY+goalPosts] do
            g.DrawLine(pen, offsetX, offsetY+i*cellSize, offsetX+width, offsetY+i*cellSize)

        for i in [0..cellsX] do
            for j in [0..cellsY] do
                match board.[i,j] with 
                | BoardElement.Ball | BoardElement.Player ->
                        drawStone g i j board.[i,j]
                | _ -> ()            

        let buttonSpacing = 10
        let buttonWidth = (width-buttonSpacing)/2
        let buttonHeight = 30
        let buttonY = offsetY+height+80

        match state with
        | State.ChooseMove ->
            let ballPositions = possibleBallPositions board
            printfn "Should allow ball moves: %i" ballPositions.Length
            drawButton form (offsetX, buttonY) "Ball" (buttonWidth, buttonHeight) (ballPositions.Length>0) (chooseMove MoveType.Ball)

            let playerPositions = possiblePlayerPositions board
            drawButton form (offsetX+buttonSpacing+buttonWidth, buttonY) "Player" (buttonWidth, buttonHeight) (playerPositions.Length>0) (chooseMove MoveType.Player)
            drawButton form (offsetX, buttonY+buttonSpacing+buttonHeight) "OK" (buttonWidth, buttonHeight) false (fun _ -> ())
        | State.MoveSelected ->
            //show possible positions only on mouse over
            ()
//            match moveType with
//            | MoveType.Ball ->
//                let positions = possibleBallPositions board
//                drawPossiblePositions g positions
//            | MoveType.Player ->
//                let positions = possiblePlayerPositions board
//                drawPossiblePositions g positions
               
        // undo button is for later
        //drawButton form (offsetX+buttonSpacing+buttonWidth, buttonY+buttonSpacing+buttonHeight) "Undo" (buttonWidth, buttonHeight)

    let initializeForm() =         
        let form = new Form(Width=width, Height=height, Visible=true, Text="Phutball", TopMost=true)
        
        form.MouseClick.Add(fun arg -> 
            let x = (arg.X-offsetX+cellSize/2)/cellSize
            let y = cellsY-(arg.Y-offsetY-cellSize/2)/cellSize
            if x>=0&&y>=0 then
                if (state=State.MoveSelected && moveAllowed board moveType (x,y)) then
                    let el = if moveType=MoveType.Ball then BoardElement.Ball else BoardElement.Player
                    //make the move
                    let gs,newBoard = move board moveType (x,y)
                    board<-newBoard
                    state<-State.ChooseMove
                    drawBoard form board
           )
        
        form.Paint.Add(fun e -> 
            printfn "Drawing"
            drawBoard form board)      
        
    member this.Start() =
        initializeForm()
        
let boardForm = new BoardForm()
boardForm.Start()
