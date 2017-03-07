
module Haskeroids.Text
    ( Text
    , mkText
    , setTextCenter
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Geometry.Transform
import Haskeroids.Render
import Haskeroids.Text.Font

import Data.List

data Text = Text
    { textBody   :: Body
    , textWidth  :: Float
    , textHeight :: Float
    , textLines  :: [LineSegment]
    }

instance LineRenderable Text where
    interpolatedLines f t = map (transform b') $ textLines t where
        b' = interpolatedBody f $ textBody t

mkText :: Font -> FontSize -> String -> Text
mkText f sz s = Text
    { textBody   = initBody (0,0) 0 (0,0) 0
    , textWidth  = sz * len + (sz*0.2) * (len - 1)
    , textHeight = sz
    , textLines  = lns
    } where
    len = fromIntegral $ length s
    lns = fst . foldl' go ([],0) . map (charLines f sz) $ s

    go (ls,x) l = (offset x l ++ ls, x + sz * 1.2)
    offset x ls = map (applyXform $ translatePt (x,0)) ls


setTextCenter :: Vec2 -> Text -> Text
setTextCenter (cx,cy) (Text b w h ls) = Text b' w h ls where
    b'  = b { bodyPos = pos, prevPos = pos }
    pos = (cx-w/2,cy-h/2)

