import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)

main : Element
main =
  collage 500 500
    [ move (-200,-200) ((cornerSplit wave 6) aFrame)    
    ]

aFrame : Frame
aFrame = {origin=(0,0), edge1=(400,0), edge2=(0,400)}

cornerSplit : Painter -> Int -> Painter 
cornerSplit painter n = 
  if (n==0)
    then painter 
    else let up          = upSplit painter (n-1)
             right       = rightSplit painter (n-1)
             topLeft     = beside up up
             bottomRight = below right right
             corner      = cornerSplit painter (n-1)
         in  beside (below painter topLeft) (below bottomRight corner)

upSplit : Painter -> Int -> Painter
upSplit painter n = 
  if (n==0) 
    then painter
    else let smaller = upSplit painter (n-1)
         in  below painter (beside smaller smaller) 

rightSplit : Painter -> Int -> Painter
rightSplit painter n = 
  if (n == 0) 
    then painter 
    else let smaller = rightSplit painter (n-1)
         in  beside painter (below smaller smaller)

flippedPairs : Painter -> Painter 
flippedPairs p = let p2 = beside p (flipVert p) 
                 in  below p2 p2

wave4 : Painter
wave4 = flippedPairs wave

wave2 : Painter 
wave2 = beside wave (flipVert wave)

-- The wave painter. Co-ordinate value taken from bill-the-lizard's solution
wave : Painter 
wave  = segmentsPainter 
  [((0.006, 0.840), (0.155, 0.591)),
   ((0.006, 0.635), (0.155, 0.392)),
   ((0.304, 0.646), (0.155, 0.591)),
   ((0.298, 0.591), (0.155, 0.392)),
   ((0.304, 0.646), (0.403, 0.646)),
   ((0.298, 0.591), (0.354, 0.492)),
   ((0.403, 0.646), (0.348, 0.845)),
   ((0.354, 0.492), (0.249, 0.000)),
   ((0.403, 0.000), (0.502, 0.293)),
   ((0.502, 0.293), (0.602, 0.000)),
   ((0.348, 0.845), (0.403, 0.999)), 
   ((0.602, 0.999), (0.652, 0.845)),
   ((0.652, 0.845), (0.602, 0.646)),
   ((0.602, 0.646), (0.751, 0.646)),
   ((0.751, 0.646), (0.999, 0.343)),
   ((0.751, 0.000), (0.597, 0.442)),
   ((0.597, 0.442), (0.999, 0.144))]

below : Painter -> Painter -> Painter 
below p1 p2 = let splitPoint     = (0, 0.5)
                  bottomPainter  = transformPainter p1 (0,0) (1,0) splitPoint
                  topPainter     = transformPainter p2 splitPoint (1,0.5) (0,1)
              in  \f -> [bottomPainter f, topPainter f] |> group

beside : Painter -> Painter -> Painter 
beside p1 p2 = let splitPoint   = (0.5, 0)
                   leftPainter  = transformPainter p1 (0,0) splitPoint (0,1)
                   rightPainter = transformPainter p2 splitPoint (1,0) (0.5,1)
               in  \f -> [leftPainter f, rightPainter f] |> group

flipHoriz : Painter -> Painter 
flipHoriz p = transformPainter p (1.0, 0.0) (0.0, 0.0) (1.0, 1.0) 

flipVert : Painter -> Painter 
flipVert p = transformPainter p (0.0, 1.0) (1.0, 1.0) (0.0, 0.0)

transformPainter : Painter -> Point -> Point -> Point -> Painter 
transformPainter painter origin corner1 corner2 = 
  \frame -> let m         = frameCoordMap frame
                newOrigin = m origin
                edge1     = subVect (m corner1) newOrigin
                edge2     = subVect (m corner2) newOrigin
                newFrame  = {origin = newOrigin, edge1 = edge1, edge2 = edge2}
            in  painter newFrame

segmentsPainter : List (Point, Point) -> Painter
segmentsPainter ps = 
  \f -> map (drawLineInsideFrame f) ps |> group

drawLineInsideFrame : Frame -> (Point, Point) -> Form
drawLineInsideFrame f (x,y) = drawLine (frameCoordMap f x) (frameCoordMap f y) 

type alias Frame = { origin: Point, edge1: Point, edge2: Point}
type alias Painter = Frame -> Form
type alias Point = (Float, Float)

frameCoordMap : Frame -> Point -> Point
frameCoordMap f = 
  \(x,y) -> addVect f.origin (addVect (scaleVect x f.edge1) (scaleVect y f.edge2))

addVect : Point -> Point -> Point
addVect (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subVect : Point -> Point -> Point
subVect (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

scaleVect : Float -> Point -> Point
scaleVect s (x,y) = (s*x, s*y)

drawLine : Point -> Point -> Form
drawLine x y = segment x y |> traced (dashed blue)
