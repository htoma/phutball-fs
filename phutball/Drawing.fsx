open System
open System.Windows.Forms 
open System.Drawing

let width = 370
let height = 600

let cellsX = 14    
let cellsY = 18
let offsetMarkersX = 20
let offsetMarkersY = 20
let offsetX = offsetMarkersX+20
let offsetY = offsetMarkersY+20
let offsetTextX = 15
let offsetTextY = 12
let cellSize = 20

let form = new Form(Width= width, Height = height, Visible = true, Text = "Phutball") 
form.TopMost <- true

let drawButton x y caption = 
    let button = new Button(Text=caption, Top=y, Left=x, BackColor=Color.Beige, Size=new Size(80,50))
    form.Controls.Add button

let drawBoard (g:Graphics) = 
    let width = cellsX*cellSize
    let height = cellsY*cellSize
    let goalPosts = 2
    let heightWithPosts = height+2*cellSize

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

    drawButton offsetX (offsetY+height+2*cellSize+2*offsetTextY) "Ball"

let drawStone (g:Graphics) x y =
    use brush = new SolidBrush(Color.Coral)
    g.FillEllipse(brush, new Rectangle(offsetX+x*cellSize-cellSize/2, offsetY+y*cellSize-cellSize/2, cellSize, cellSize))

form.Paint.Add(
    fun e -> drawBoard e.Graphics)

form.MouseClick.Add(fun arg -> 
        let x = (arg.X-offsetX+cellSize/2)/cellSize
        let y = (arg.Y-offsetY-cellSize/2)/cellSize
        let g = form.CreateGraphics()
        if x>=0 && x<=cellsX && y>=0 && y<=cellsY then
            drawStone g x (y+1)
        form.Text <- sprintf "Clicked at: %i, %i" x y)
    

