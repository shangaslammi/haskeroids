
module Haskeroids.Render where

import Graphics.Rendering.OpenGL
import Haskeroids.Util

class LineRenderable r where
    lineSegments :: r -> [LineSegment]

    render :: r -> IO ()
    render = renderLines . lineSegments

    
renderLines :: [LineSegment] -> IO ()
renderLines lns = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    renderPrimitive Lines $ mapM_ lineVertices lns

lineVertices :: LineSegment -> IO ()
lineVertices (LineSegment ((x,y),(x',y'))) = do
    vertex $ Vertex2 x y
    vertex $ Vertex2 x' y'