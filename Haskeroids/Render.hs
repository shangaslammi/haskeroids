
module Haskeroids.Render (LineRenderable(..)) where

import Graphics.Rendering.OpenGL
import Haskeroids.Geometry

-- | Object that can be rendered as a group of lines
class LineRenderable r where
    lineSegments :: r -> [LineSegment]

    interpolatedLines :: Float -> r -> [LineSegment]
    
    render :: r -> IO ()
    render = renderLines . lineSegments
    
    renderInterpolated :: Float -> r -> IO()
    renderInterpolated f = renderLines . interpolatedLines f


-- | Render a list of line segments using OpenGL
renderLines :: [LineSegment] -> IO ()
renderLines lns = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    renderPrimitive Lines $ mapM_ lineVertices lns

-- | Generate the OpenGL vertices of a line segment
lineVertices :: LineSegment -> IO ()
lineVertices (LineSegment (p,p')) = do
    ptVertex p
    ptVertex p'

-- | Generate an OpenGL vertex from a point
ptVertex :: Vec2 -> IO ()
ptVertex = vertex . uncurry Vertex2
