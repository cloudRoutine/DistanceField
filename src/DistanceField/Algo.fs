module Algo

open System.Threading.Tasks

let getBorder (isInside:bool[,], xRes, yRes) =
  [|for x = 1 to xRes-2 do 
      for y = 1 to yRes-2 do
        let p = isInside.[x,y]
        
        let square = 
            isInside.[x,y-1] = p && 
            isInside.[x,y+1] = p && 
            isInside.[x-1,y] = p && 
            isInside.[x+1,y] = p 

        let diagonal = 
            isInside.[x-1,y-1] = p && 
            isInside.[x-1,y+1] = p && 
            isInside.[x+1,y-1] = p && 
            isInside.[x+1,y+1] = p

        if not (square && diagonal) then yield (x,y)|]
  |> Array.partition (fun (x,y) -> isInside.[x,y])

let borderAsYSortedArray border yRes = 
  let lineBorder = Array.create yRes [||]
  border
  |> Array.groupBy snd
  |> Array.iter (fun (y,xs) -> lineBorder.[y] <- Array.map fst xs)
  lineBorder

let inline dist x y = (x*x) + (y*y)

// Brute force, can double in performance with small changes and/or
// when moved around in different files for no clear reason
let genField (xRes, yRes, border, isInside:bool[,]) = 
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
let fastGenFieldOld (xRes, yRes, border, isInside:bool[,]) = 
  let intBorder = borderAsYSortedArray (fst border) yRes
  let extBorder = borderAsYSortedArray (snd border) yRes

  let searchLine x y line =
    let xs = if isInside.[x,y] then intBorder.[line] else extBorder.[line]
    let mutable best = 1000000.0
    let y2 = (float y - float line)
    for xx in xs do best <- min best (dist (float x - float xx) y2)
    best
  
  let searchField x y =
    let rec loop i lim best = 
        if i >= lim then best else 
        let y1 = y - i
        let y2 = y + i
        let a = if (y1 >= 0) && (y1 < yRes) then searchLine x y y1 else 1000000.0
        let b = if (y2 >= 0) && (y2 < yRes) && (y1<>y2) then searchLine x y y2 else 1000000.0
        let possiblebest = min a b
        if possiblebest >= best then loop (i+1) lim best else
        let lim' = (int ((float i) + (sqrt possiblebest))) // Incorrect calculation of limit
        loop (i+1) lim' possiblebest
           
    loop 0 (max (yRes-y) y) 1000000.

  let field = Array2D.zeroCreate xRes yRes
  Parallel.For (0, yRes,
    fun y -> 
      for x = 0 to xRes-1 do
        field.[x,y] <- sqrt (searchField x y)  
  ) |> ignore
  field


let fastGenField (xRes, yRes ,border, isInside:bool[,]) = 
  let intBorder = borderAsYSortedArray (fst border) yRes
  let extBorder = borderAsYSortedArray (snd border) yRes

  let inline searchLine x y line (border:int[][]) =
    let mutable best = System.Int32.MaxValue
    if (line >= 0) && (line < yRes) then
      for xx in border.[line] do 
        best <- min best ((x - xx) * (x - xx) + (y - line) * (y - line))
    best
 
  let searchField x y border =
    let rec loop i limit best =
      if i > limit then best else 
      let closest = min (searchLine x y (y + i) border) (searchLine x y (y - i) border)
      if  closest < best
      then loop (i+1) (int (sqrt (float closest))) closest
      else loop (i+1) limit best
           
    loop 0 (max (yRes-y) y) System.Int32.MaxValue

  let output = Array2D.zeroCreate xRes yRes
  Parallel.For (0, yRes,
    fun y -> 
      for x = 0 to xRes-1 do
        let inside = isInside.[x,y]
        output.[x,y] <- sqrt (float (searchField x y (if inside then intBorder else extBorder)))
  ) |> ignore
  output
