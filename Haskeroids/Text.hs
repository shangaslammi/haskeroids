
module Haskeroids.Text where

import Haskeroids.Geometry
import Haskeroids.Geometry.Body
import Haskeroids.Geometry.Transform
import Haskeroids.Render
import Haskeroids.Text.Font

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
    , textWidth  = sz * len + (sz*0.1) * (len - 1)
    , textHeight = sz
    , textLines  = lns
    } where
    len = fromIntegral $ length s
    lns = fst . foldr go ([],0) . map (charLines f sz) $ s

    go l (ls,x) = (offset x l ++ ls, x + sz * 1.1)
    offset x ls = map (applyXform $ translatePt (x,0)) ls


setCenterPos :: Vec2 -> Text -> Text
setCenterPos = undefined

