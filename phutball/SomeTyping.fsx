#r @"C:\work\phutball\phutball\bin\Debug\phutball.exe"
open Board

open System
open System.Windows.Forms 
open System.Drawing

type CompositeForm () =
   inherit Form()
   override this.CreateParams = 
      let cp = base.CreateParams
      cp.ExStyle <- cp.ExStyle ||| 0x02000000
      cp

type State =
    | ChooseMove
    | MoveSelected
    | GameOver of Team

type BoardForm() = 
    //constants
    let formWidth = 370
    let formHeight = 630
    let cellsX = 14    
    let cellsY = 18
    let offsetMarkersX = 20
    let offsetMarkersY = 20
    let offsetX = offsetMarkersX+20
    let offsetY = offsetMarkersY+20
    let offsetTextX = 15
    let offsetTextY = 12
    let cellSize = 20
    let width = cellsX*cellSize
    let height = cellsY*cellSize
    let goalPosts = 2
    let heightWithPosts = height+2*cellSize

    let initBoard () = 
        initializeBoard (cellsX+1,cellsY+1)

    let mutable board = initBoard()
    let mutable state = State.ChooseMove
    let mutable moveType = MoveType.Player

    let buttonBall = new Button(BackColor=Color.Beige, Enabled=false)
    let buttonPlayer = new Button(BackColor=Color.Beige, Enabled=false)
    let buttonUndo = new Button(BackColor=Color.Beige, Enabled=false)
    let buttonRestart = new Button(BackColor=Color.Beige, Enabled=true)    

    let initializeButton (button:Button) left top caption sizeX sizeY enabled callback = 
        button.Text<-caption
        button.Top<-top
        button.Left<-left
        button.Size<-new Size(sizeX,sizeY)
        button.Click.Add(callback)

    let drawingPositionFromCell x y =
        new Rectangle(offsetX+x*cellSize-cellSize/2, offsetY+(cellsY-y+1)*cellSize-cellSize/2, cellSize, cellSize)

    let drawElement (g:Graphics) x y (color:Color) =
        use brush = new SolidBrush(color)
        g.FillEllipse(brush, drawingPositionFromCell x y)

    let load (file:string) =
       let path = IO.Path.Combine(__SOURCE_DIRECTORY__, "images", file)
       Image.FromFile path

    let drawImage (g:Graphics) (image:Image) (x:int) (y:int) =
        g.DrawImage(image, drawingPositionFromCell x y)

    let drawPlayer (g:Graphics) x y =
        let image = load "player1.png"
        drawImage g image x y 

    let drawStone (g:Graphics) x y (el:BoardElement) =
        if el=BoardElement.Ball then
            drawElement g x y Color.Coral
        else
            drawPlayer g x y

    let drawText (g:Graphics) (text:string) (rect:Rectangle) (font:Font) (color:Color) =
        let flags = TextFormatFlags.HorizontalCenter ||| TextFormatFlags.VerticalCenter ||| TextFormatFlags.WordBreak
        TextRenderer.DrawText(g, text, font, rect, color, flags)
        g.DrawRectangle(Pens.Black, rect)

    let drawPossiblePositions (g:Graphics) (positions:(int*int) list) =
        positions
        |> List.iter (fun (i,j) -> drawElement g i j Color.AntiqueWhite)

    let drawGoal (g:Graphics) (team: Team) =
        use font = new Font("Arial", 12.f, FontStyle.Bold)
        use brush = new SolidBrush(Color.Blue)
        let y = if team=Team.Down then 0 else cellsY+1
        let rect = Rectangle(offsetX, offsetY+cellSize*y, width, cellSize)
        drawText g "GOAL !!!" rect font Color.Blue

    let drawMoveMessage (g:Graphics) (rect:Rectangle) (text:string) = 
        use pen = new Pen(Brushes.Black)
        use font = new Font("Arial", 10.f)
        g.DrawRectangle(pen, offsetX,  offsetY+heightWithPosts+25, width, 2*cellSize)
        drawText g text rect font Color.IndianRed
       
    let drawBoard (form:Form) (board:BoardElement[,]) =   
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

        let rect = new Rectangle(offsetX,  offsetY+heightWithPosts+25, width, 2*cellSize)        

        match state with
        | State.ChooseMove ->
            let ballPositions = possibleBallPositions board
            buttonBall.Enabled<-(ballPositions.Length>0)
            let playerPositions = possiblePlayerPositions board
            buttonPlayer.Enabled<-(playerPositions.Length>0)
            drawMoveMessage g rect "Choose move"            
        | State.MoveSelected ->
           drawMoveMessage g rect (sprintf "Move selected: %A" moveType)
        | State.GameOver team->
            drawGoal g team
            buttonBall.Enabled<-false
            buttonPlayer.Enabled<-false
            buttonRestart.Enabled<-true
            drawMoveMessage g rect "Game over"

    let chooseMove (form:Form) (move:MoveType) =
        fun _ ->
            moveType<-move
            state<-State.MoveSelected
            drawBoard form board

    let reset (form:Form) =
        state<-State.ChooseMove
        buttonBall.Enabled<-false
        buttonPlayer.Enabled<-false
        board<-initBoard()
        drawBoard form board    

    let drawButtons (form:Form) =
        let buttonSpacing = 5
        let buttonWidth = (width-buttonSpacing)/2
        let buttonHeight = 30
        let buttonY = offsetY+height+110

        initializeButton buttonBall offsetX buttonY "Ball" buttonWidth buttonHeight false (chooseMove form MoveType.Ball)
        initializeButton buttonPlayer (offsetX+buttonSpacing+buttonWidth) buttonY "Player" buttonWidth buttonHeight false (chooseMove form MoveType.Player)
        initializeButton buttonUndo offsetX (buttonY+buttonSpacing+buttonHeight) "Undo" buttonWidth buttonHeight false (fun _ -> ())
        initializeButton buttonRestart (offsetX+buttonSpacing+buttonWidth) (buttonY+buttonSpacing+buttonHeight) "Restart" buttonWidth buttonHeight false (fun _ -> reset form)
        [buttonBall; buttonPlayer; buttonUndo; buttonRestart] |> Seq.cast<Control> |> Array.ofSeq |> form.Controls.AddRange

    let cellFromClick x y =
        let i = (x-offsetX+cellSize/2)/cellSize
        let tmp = offsetY+cellSize/2
        let j = 
            if y-cellSize/2>offsetY then cellsY-(y-offsetY-cellSize/2)/cellSize
            else cellsY+1
        i,j

    let initializeForm() =         
        let form = new CompositeForm(Width=formWidth, Height=formHeight, Visible=true, Text="Phutball", TopMost=true)
        
        form.MouseClick.Add(fun arg -> 
            let x,y = cellFromClick arg.X arg.Y
            if x<0 || x>cellsX || y<(-1) || y>cellsY+1 then ()
            else
                if (state=State.MoveSelected && moveAllowed board moveType (x,y)) then
                    let el = if moveType=MoveType.Ball then BoardElement.Ball else BoardElement.Player
                    let gs,newBoard = move board moveType (x,y)
                    match gs with 
                    | GameState.On ->
                        state<-State.ChooseMove
                        board<-newBoard
                    | GameState.Goal team ->
                        state<-State.GameOver team
                    drawBoard form board
                )

        form.Paint.Add(fun e -> 
            drawBoard form board) 
            
        drawButtons form    
        
    member this.Start() =
        initializeForm()
        
let boardForm = new BoardForm()
boardForm.Start()

