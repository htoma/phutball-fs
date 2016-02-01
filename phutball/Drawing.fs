module Drawing
    open Board
    open Game

    open System
    open System.Windows.Forms
    open System.Drawing

    type CompositeForm () =
       inherit Form()
       override this.CreateParams = 
          let cp = base.CreateParams
          cp.ExStyle <- cp.ExStyle ||| 0x02000000
          cp

    type BoardForm() = 
        //constants
        let formWidth = 370
        let formHeight = 630
        let offsetMarkersX = 20
        let offsetMarkersY = 20
        let offsetX = offsetMarkersX+20
        let offsetY = offsetMarkersY+20
        let offsetTextX = 15
        let offsetTextY = 12
        let cellSize = 20
        let goalPosts = 2
    
        let game = Game()

        let width = game.CellsX*cellSize
        let height = game.CellsY*cellSize
        let heightWithPosts = height+2*cellSize

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
            new Rectangle(offsetX+x*cellSize-cellSize/2, offsetY+(game.CellsY-y+1)*cellSize-cellSize/2, cellSize, cellSize)

        let drawElement (g:Graphics) x y (color:Color) =
            use brush = new SolidBrush(color)
            g.FillEllipse(brush, drawingPositionFromCell x y)

        let load (file:string) =
           let path = IO.Path.Combine(__SOURCE_DIRECTORY__, "images", file)
           Image.FromFile path

        let drawImage (g:Graphics) (image:Image) (x:int) (y:int) =
            g.DrawImage(image, drawingPositionFromCell x y)

        let drawPlayer (g:Graphics) x y =
            //note(htoma): uncomment for drawing player icon instead of stone (more player images can be randomized)
            //let image = load "player1.png"
            //drawImage g image x y
            drawElement g x y Color.DarkSlateGray

        let drawBall (g:Graphics) x y =
            let image = load "ball.png"
            drawImage g image x y

        let drawPlayerBall (g:Graphics) x y (el:BoardElement) =
            if el=BoardElement.Ball then
                drawBall g x y            
            else
                drawPlayer g x y

        let drawText (g:Graphics) (text:string) (rect:Rectangle) (font:Font) (color:Color) =
            let flags = TextFormatFlags.HorizontalCenter ||| TextFormatFlags.VerticalCenter ||| TextFormatFlags.WordBreak
            TextRenderer.DrawText(g, text, font, rect, color, flags)
            //g.DrawRectangle(Pens.Black, rect)

        let drawPossiblePositions (g:Graphics) (positions:(int*int) list) =
            positions
            |> List.iter (fun (i,j) -> drawElement g i j Color.AntiqueWhite)

        let drawGoal (g:Graphics) (team: Team) =
            use font = new Font("Arial", 12.f, FontStyle.Bold)
            use brush = new SolidBrush(Color.Blue)
            let y = if team=Team.Down then 0 else game.CellsY+1
            let rect = Rectangle(offsetX, offsetY+cellSize*y, width, cellSize)
            use goalBrush = new SolidBrush(Color.LawnGreen)
            g.FillRectangle(goalBrush, rect)
            drawText g "GOAL !!!" rect font Color.Blue

        let drawMoveMessage (g:Graphics) (rect:Rectangle) (text:string) = 
            let image = load (if game.Turn=Turn.First then "arrow_up.png" else "arrow_down.png")
            let y = rect.Top+rect.Height/2-image.Height/2
            g.DrawImage(image, rect.Left, y)
            let offset = image.Width+3

            use pen = new Pen(Brushes.Black)
            use font = new Font("Arial", 10.f)
            drawText g text (new Rectangle(rect.Left+offset, rect.Top, rect.Width-offset, rect.Height)) font Color.DarkSlateBlue
       
        let drawBoard (form:Form) (board:BoardElement[,]) =   
            use g = form.CreateGraphics()
            g.Clear(Color.Beige)
            use font = new Font("Arial", 10.f)
            use textBrush = new SolidBrush(Color.Black)
            for i in [game.CellsY+2..-1..0] do
                g.DrawString(sprintf "%2i" i, font, textBrush, float32 offsetMarkersX, float32 (offsetMarkersY+offsetTextY+cellSize*(game.CellsY+2-i)))
                g.DrawString(sprintf "%2i" i, font, textBrush, float32 (offsetX+width), float32 (offsetMarkersY+offsetTextY+cellSize*(game.CellsY+2-i)))
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
            for i in [0..game.CellsX] do
                g.DrawLine(pen, offsetX+i*cellSize, offsetY, offsetX+i*cellSize, offsetY+heightWithPosts)
            for i in [0..game.CellsY+goalPosts] do
                g.DrawLine(pen, offsetX, offsetY+i*cellSize, offsetX+width, offsetY+i*cellSize)

            for i in [0..game.CellsX] do
                for j in [0..game.CellsY] do
                    match board.[i,j] with 
                    | BoardElement.Ball | BoardElement.Player ->
                            drawPlayerBall g i j board.[i,j]
                    | _ -> ()

            let rect = new Rectangle(offsetX,  offsetY+heightWithPosts+25, width, 2*cellSize)        

            let player = (if game.Turn=Turn.First then 1 else 2)
            match game.State with
            | State.ChooseMove ->
                let moveBallAllowed = game.MoveAllowed board MoveType.Ball 
                buttonBall.Enabled<-moveBallAllowed
                let movePlayerAllowed = game.MoveAllowed board MoveType.Player 
                buttonPlayer.Enabled<-movePlayerAllowed
                drawMoveMessage g rect (sprintf "Player %i, Select Move" player)        
            | State.MoveSelected ->
               drawMoveMessage g rect (sprintf "Player %i, Move selected: %A" player game.Move)
            | State.GameOver team->
                drawGoal g team
                buttonBall.Enabled<-false
                buttonPlayer.Enabled<-false
                buttonRestart.Enabled<-true
                drawMoveMessage g rect "Game over"

        let chooseMove (form:Form) (move:MoveType) =
            fun _ ->
                game.ChooseMove move
                drawBoard form game.Board

        let reset (form:Form) =
            game.Reset()
            buttonBall.Enabled<-false
            buttonPlayer.Enabled<-false        
            drawBoard form game.Board   

        let drawButtons (form:Form) =
            let buttonSpacing = 5
            let buttonWidth = (width-buttonSpacing)/2
            let buttonHeight = 30
            let buttonY = offsetY+height+110

            initializeButton buttonPlayer offsetX buttonY "Player" buttonWidth buttonHeight false (chooseMove form MoveType.Player)
            initializeButton buttonBall (offsetX+buttonSpacing+buttonWidth) buttonY "Ball" buttonWidth buttonHeight false (chooseMove form MoveType.Ball)        
            initializeButton buttonUndo offsetX (buttonY+buttonSpacing+buttonHeight) "Undo" buttonWidth buttonHeight false (fun _ -> ())
            initializeButton buttonRestart (offsetX+buttonSpacing+buttonWidth) (buttonY+buttonSpacing+buttonHeight) "Restart" buttonWidth buttonHeight false (fun _ -> reset form)
            [buttonBall; buttonPlayer; buttonUndo; buttonRestart] |> Seq.cast<Control> |> Array.ofSeq |> form.Controls.AddRange

        let cellFromClick x y =
            let i = (x-offsetX+cellSize/2)/cellSize
            let tmp = offsetY+cellSize/2
            let j = 
                if y-cellSize/2>offsetY then game.CellsY-(y-offsetY-cellSize/2)/cellSize
                else game.CellsY+1
            i,j

        let initializeForm() =         
            let form = new CompositeForm(Width=formWidth, Height=formHeight, Visible=true, Text="Phutball", TopMost=true)
        
            form.MouseClick.Add(fun arg -> 
                let x,y = cellFromClick arg.X arg.Y
                game.CellSelected (x,y)
                drawBoard form game.Board
                )

            form.Paint.Add(fun e -> 
                drawBoard form game.Board) 
            
            drawButtons form    
        
        member this.Start() =
            initializeForm()
