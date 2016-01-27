module Helper

  open System
  open System.Windows.Media
  open System.Windows.Media.Imaging
  open System.Diagnostics
  
  let time f x =
    let s = new Stopwatch()
    s.Start()
    let r = f x
    s.Stop()
    printfn "Time: %f" s.Elapsed.TotalSeconds
    r


  let getBitmap imgPath =
    let bmi = BitmapImage (System.Uri (imgPath))
    bmi.CreateOptions <- BitmapCreateOptions.None
    let bms =
      if bmi.Format <> PixelFormats.Bgr32
      then FormatConvertedBitmap (bmi, PixelFormats.Bgr32, null, 0.0) :> BitmapSource
      else bmi :> BitmapSource
    (bms.PixelWidth,bms.PixelHeight,bms)


  let getPixels imgPath =
    let (xRes,yRes,bms) = getBitmap imgPath
    let pixels = Array.create (xRes * yRes) 0
    bms.CopyPixels (pixels, xRes * sizeof<int32>, 0)
    (xRes,yRes,pixels)


  let print x = printfn "%A" x
  let show x = print x; x
