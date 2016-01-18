open System
open System.Windows.Forms 
open System.Drawing

let width = 370
let height = 500

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

let drawBoard (g:Graphics) = 
    let width = cellsX*cellSize
    let height = cellsY*cellSize
    let goalPosts = 2
    let heightWithPosts = height+2*cellSize

    use font = new Font("Arial", 10.f)
    use textBrush = new SolidBrush(Color.Black)
    for i in [cellsY+1..-1..1] do
        g.DrawString(sprintf "%2i" i, font, textBrush, float32 offsetMarkersX, float32 (offsetMarkersY+offsetTextY+cellSize*(cellsY+1-i)))
        g.DrawString(sprintf "%2i" i, font, textBrush, float32 (offsetX+width), float32 (offsetMarkersY+offsetTextY+cellSize*(cellsY+1-i)))
    ["A";"B";"C";"D";"E";"F";"G";"H";"J";"K";"L";"M";"N";"O";"P"]
    |> List.iteri (fun i v -> 
        g.DrawString(v, font, textBrush, float32 (offsetMarkersX+offsetTextX+i*cellSize), float32 offsetMarkersY)
        g.DrawString(v, font, textBrush, float32 (offsetMarkersX+offsetTextX+i*cellSize), float32 (offsetY+height+5)))
    
    use brush = new SolidBrush(Color.Beige)
    g.FillRectangle(brush, offsetX, offsetY, width, heightWithPosts)

    use pen = new Pen(Brushes.Black)
    for i in [0..cellsX] do
        g.DrawLine(pen, offsetX+i*cellSize, offsetY, offsetX+i*cellSize, offsetY+heightWithPosts)
    for i in [0..cellsY+goalPosts] do
        g.DrawLine(pen, offsetX, offsetY+i*cellSize, offsetX+width, offsetY+i*cellSize)

let fillEllipse (g:Graphics) x y =
    use brush = new SolidBrush(Color.Coral)
    g.FillEllipse(brush, new Rectangle(offsetX+x*cellSize-cellSize/2, offsetY+y*cellSize-cellSize/2, cellSize, cellSize))

form.Paint.Add(
    fun e -> drawBoard e.Graphics)

form.MouseClick.Add(fun arg -> 
        let x = (arg.X-offsetX)/cellSize
        let y = (arg.Y-offsetY)/cellSize
        let g = form.CreateGraphics()
        fillEllipse g x y
        form.Text <- sprintf "form clicked at: %i, %i" x y)
