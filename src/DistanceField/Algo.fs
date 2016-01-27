module Algo

open System.Numerics

let getBorder ((bm:bool[,]),xRes,yRes) =
    [for x = 1 to xRes-2 do 
      for y = 1 to yRes-2 do
        let p = bm.[x,y]
        let square = bm.[x,y-1]=p && bm.[x,y+1]=p && bm.[x-1,y]=p && bm.[x+1,y]=p 
        let diagonal = bm.[x-1,y-1]=p && bm.[x-1,y+1]=p && bm.[x+1,y-1]=p && bm.[x+1,y+1]=p
        if not (square && diagonal) then yield (x,y)]
    |> List.partition (fun (x,y) -> bm.[x,y])

let getBorderArr ((bm:bool[,]),xRes,yRes) =
    [|  for x = 1 to xRes-2 do 
          for y = 1 to yRes-2 do
            let p = bm.[x,y]
            let square = bm.[x,y-1]=p && bm.[x,y+1]=p && bm.[x-1,y]=p && bm.[x+1,y]=p 
            let diagonal = bm.[x-1,y-1]=p && bm.[x-1,y+1]=p && bm.[x+1,y-1]=p && bm.[x+1,y+1]=p
            if not (square && diagonal) then yield (x,y)
    |] |> Array.partition (fun (x,y) -> bm.[x,y])


//let getBorder ((bm:Vector2),xRes,yRes) =
//    [for x = 1 to xRes-2 do 
//      for y = 1 to yRes-2 do
//        let p = bm.[x,y]
//        let square = bm.[x,y-1]=p && bm.[x,y+1]=p && bm.[x-1,y]=p && bm.[x+1,y]=p 
//        let diagonal = bm.[x-1,y-1]=p && bm.[x-1,y+1]=p && bm.[x+1,y-1]=p && bm.[x+1,y+1]=p
//        if not (square && diagonal) then yield (x,y)]
//    |> List.partition (fun (x,y) -> bm.[x,y])

let borderAsArray b yRes = 
  let lineBorder = Array.init yRes (fun _ -> [])
  List.groupBy (fun (x,y) -> y) b
  |> List.map (fun (y,xs) -> (y,List.map fst xs))
  |> List.iter (fun (y,xs) -> lineBorder.[y] <- xs)
  lineBorder


let inline dist x y = (x*x) + (y*y)


// Brute force, can double in performance with small changes and/or
// when moved around in different files for no clear reason
let genField (xRes,yRes,border,(isInside:bool[,])) = 
  let field = Array2D.zeroCreate xRes yRes
  let (intBorder,extBorder) = border
  for y = 0 to yRes-1 do
    for x = 0 to xRes-1 do
      let mutable s = 1000000.0
      let c = if isInside.[x,y] then intBorder else extBorder
      for (xx,yy) in c do
        s <- min s (dist (float x - float xx) (float y - float yy))
      field.[x,y] <- sqrt s
  field


// If a point is found n units away, then stop searching when reaching n lines away,
// a line is one unit tall so no closer points can be found after this line
let fastGenField (xRes,yRes,border,(isInside:bool[,])) = 
  let intBorder = borderAsArray (fst border) yRes
  let extBorder = borderAsArray (snd border) yRes

  let searchLine x y line =
    let xs = if isInside.[x,y] then intBorder.[line] else extBorder.[line]
    let mutable best = 1000000.0
    let y2 = (float y - float line)
    for xx in xs do best <- min best (dist (float x - float xx) y2)
    best
  
  let searchField x y =
    let clipy y = min (yRes-1) y

    let rec loop i lim best = 
      if i >= lim then best
      else let y1 = y - i
           let y2 = y + i
           let a = if (y1 >= 0) && (y1 < yRes) then searchLine x y y1 else 1000000.0
           let b = if (y2 >= 0) && (y2 < yRes) && (y1<>y2) then searchLine x y y2 else 1000000.0
           let possiblebest = min a b
           if possiblebest < best
           then
             let lim' = (int ((float i) + (sqrt possiblebest))) // Rounding may be questionable
             loop (i+1) lim' possiblebest
           else loop (i+1) lim best
           
    loop 0 (max (yRes-y) y) 1000000.

  let field = Array2D.zeroCreate xRes yRes
  for y = 0 to yRes-1 do
    for x = 0 to xRes-1 do
      field.[x,y] <- sqrt (searchField x y)
  field