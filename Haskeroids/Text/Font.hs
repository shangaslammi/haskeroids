
module Haskeroids.Text.Font
    ( loadFont
    , charLines
    ) where

import Haskeroids.Geometry

import System.IO
import Control.Monad (liftM2)
import Control.Monad.State
import qualified Data.Set as S


data Font = Font
type FontSize = Float
type ParsedGlyph = ((Int,Int),[Int],[LineDef])
type LineDef = ((Int,Int), (Char,Int))

loadFont :: FilePath -> IO Font
loadFont = fmap parseFont . readFile

parseFont :: String -> Font
parseFont s = undefined

charLines :: Font -> FontSize -> Char -> [LineSegment]
charLines = undefined

analyzeGlyph :: [String] -> ParsedGlyph
analyzeGlyph l = ((w,h),cols,lns) where
    w    = length l
    h    = length (head l)
    pts  = liftM2 (,) [0..h-1] [0..w-1]
    lns  = evalState (st []) $ S.fromList pts
    cols = map colWidth l

    colWidth c  = if any wideChar c then 1 else 0
    wideChar ch = ch `elem` "_\\/"

    st result = do
        set <- get
        if S.null set
            then return result
            else do
                let p = S.findMin set
                s <- streakFrom p
                if null s then st result else st ((p,(head s, length s)):result)

    streakFrom p = do
        let c  = charAt p
            p' = next c p
        rm p
        case c of
            ' ' -> return []
            _
                | out p' || charAt p' /= c -> return [c]
                | otherwise -> fmap (c:) $ streakFrom p'

    out (r,c) = c < 0 || r >= h || c >= w

    next ch (r,c) = (r+r',c+c') where
        (r',c') = lineDir ch

    charAt (r,c) = (l !! c) !! r
    rm p = modify $ S.delete p

mkCharLines :: ParsedGlyph -> [LineSegment]
mkCharLines ((w,h),cols,lns) = map mkLine lns where
    mkLine (sp,(d,l)) = LineSegment ((startPoint sp d),(endPoint sp d l))
    startPoint (r,c) d   = case d of
        '_' -> pCoord (r+1,c)
        '/' -> pCoord (r, c+1)
        _   -> pCoord (r,c)

    endPoint sp d 0 = startPoint sp d
    endPoint (r,c) d l = endPoint (r+r',c+c') d (l-1) where
        (r',c') = lineDir d

    pCoord (r,c) = (x,y) where
        x = fromIntegral (colSlot c) / fromIntegral totalSlots
        y = fromIntegral (r-1) / fromIntegral (h-1)
    colSlot c  = sum $ take c $ cols
    totalSlots = max 1 $ sum cols

lineDir :: Char -> (Int, Int)
lineDir c = case c of
    '|'  -> (1, 0)
    '_'  -> (0, 1)
    '\\' -> (1, 1)
    '/'  -> (1,-1)
