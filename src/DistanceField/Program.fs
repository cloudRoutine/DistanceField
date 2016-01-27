module Main

open System
open System.Windows
open System.Windows.Forms
open System.Drawing

open Helper
open Algo


[<EntryPoint>]
[<System.STAThread>]
let main argv =
    let (xRes,yRes,rawpix) = getPixels (Application.StartupPath+"""\r512.png""")
    printfn "Size %A %A" xRes yRes

    let isInside = Array2D.zeroCreate xRes yRes
    let picture = Array2D.zeroCreate xRes yRes
    let field = Array2D.zeroCreate xRes yRes

    for y = 0 to yRes - 1 do
      for x = 0 to xRes - 1 do
        let p = rawpix.[x+y*xRes] &&& 0xff
        if p = 0 then isInside.[x,y] <- true 
        picture.[x,y] <- p &&& 0xff



    let bm = new Bitmap(Application.StartupPath+"""\r512.png""")
    let pb = new PictureBox()

    let border = getBorder (isInside,xRes,yRes)
    printfn "Inside border length  %A" (fst border).Length
    printfn "Outside border length %A" (snd border).Length
   

    let updatebm() =
      for y = 0 to yRes-1 do
        for x = 0 to xRes-1 do
          let p = int32 (max 0.0 (min (field.[x,y]) 255.0))
          if isInside.[x,y]
          then bm.SetPixel(x,y,Color.FromArgb(255,p,0,0))
          else bm.SetPixel(x,y,Color.FromArgb(255,p,p,p))


    let redraw() =
      updatebm()
      pb.Image <- bm
      Application.DoEvents()


    let source = System.Windows.Media.Imaging.WriteableBitmap(xRes, yRes, 1.0, 1.0, Media.PixelFormats.Bgr32, null)
    source.WritePixels(Int32Rect(0, 0, xRes, yRes), rawpix, xRes * sizeof<int32>, 0)
    
    let ci = Controls.Image(Source=source, Margin=Thickness 5.0)
    let frm = new Form(ClientSize=Drawing.Size(800,600))
    pb.Image <- bm
    pb.SizeMode <- PictureBoxSizeMode.AutoSize
    frm.Controls.Add(pb)
    frm.Show()


    printf "Fast algorithm..."
    let f1 = time fastGenField (xRes,yRes,border,isInside)
    Array2D.iteri (fun x y d -> field.[x,y] <- d) f1
    redraw()     
    
    printf "Slow algorithm..."
    let f2 = time genField (xRes,yRes,border,isInside)
    Array2D.iteri (fun x y d -> field.[x,y] <- d) f2
    redraw()     

    f2 |> Array2D.iteri (fun x y d -> if f1.[x,y] <> d then failwithf "Failed at: (%A,%A) - %A<>%A " x y d f1.[x,y])
    printf "Both distance-fields are identical"

    while frm.Created do
      pb.Image <- bm
      Application.DoEvents()
      System.Threading.Thread.Sleep(100)
       
    //printfn "Press any key to continue!"
    //Console.ReadKey() |> ignore
    0
