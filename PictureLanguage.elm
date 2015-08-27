import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)

--main = show (frameCoordMap aFrame (1,1))

main : Element
main =
  collage 480 640
    [ move (-200,0) (outlines aFrame)
    , move (-200,0) (bigX aFrame)
    , move (-200,0) (diamond aFrame)
    ]

lines : List Form 
lines = [(drawLine (0,0) (100,100)), (drawLine (0,10) (100,10)), (drawLine (10,0) (10,100))]

aFrame : Frame
aFrame = {origin=(0,0), edge1=(0,300), edge2=(400,0)}

outlines : Frame -> Form
outlines f = segmentsPainter [((0,0), (0,1)), ((0,1), (1,1)), ((1,1), (1,0)), ((1,0),(0,0))] f 

bigX : Frame -> Form
bigX f = segmentsPainter [((0,0), (1,1)), ((1,0),(0,1))] f 

diamond : Frame -> Form
diamond f = segmentsPainter 
  [((0.5,0), (1,0.5)), ((1,0.5), (0.5,1)), ((0.5,1), (0,0.5)), ((0,0.5),(0.5,0))] f 

segmentsPainter : List ((Float, Float), (Float, Float)) -> Frame -> Form
segmentsPainter ps = 
  \f -> map (drawLineInsideFrame f) ps |> group

drawLineInsideFrame : Frame -> ((Float, Float), (Float, Float)) -> Form
drawLineInsideFrame f (x,y) = drawLine (frameCoordMap f x) (frameCoordMap f y) 

type alias Frame = { origin: (Float,Float), edge1: (Float, Float), edge2: (Float, Float)}

frameCoordMap : Frame -> (Float, Float) -> (Float, Float)
frameCoordMap f = 
  \(x,y) -> addVect f.origin (addVect (scaleVect x f.edge1) (scaleVect y f.edge2))

addVect : (Float, Float) -> (Float, Float) -> (Float, Float)
addVect (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subVect : (Float, Float) -> (Float, Float) -> (Float, Float)
subVect (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

scaleVect : Float -> (Float, Float) -> (Float, Float)
scaleVect s (x,y) = (s*x, s*y)

drawLine : (Float, Float) -> (Float, Float) -> Form
drawLine x y = segment x y |> traced (dashed blue)
