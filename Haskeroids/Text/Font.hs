
module Haskeroids.Text.Font
    ( loadFont
    , charLines
    ) where

import Haskeroids.Geometry
import Haskeroids.Geometry.Transform

import System.IO
import Control.Monad (liftM2)
import Control.Monad.State
import Control.Arrow (second)
import Data.List
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

newtype Font = Font { toMap :: Map Char Glyph }
type Glyph = [LineSegment]

type FontSize = Float

type GlyphInfo = ((Int,Int),[Int],[LineDef])
type LineDef = ((Int,Int), (Char,Int))

loadFont :: FilePath -> IO Font
loadFont = fmap parseFont . readFile

parseFont :: String -> Font
parseFont s = Font $ M.fromList assocs where
    assocs = zip codepage glyphs
    (codepage:lns) = lines s
    glyphs = map (mkCharLines . analyzeGlyph) $ splitChars lns

charLines :: Font -> FontSize -> Char -> [LineSegment]
charLines (Font m) sz c = scaleLines sz $ M.findWithDefault [] c m

scaleLines :: FontSize -> [LineSegment] -> [LineSegment]
scaleLines sz = map $ applyXform (sz */)

splitChars :: [String] -> [[String]]
splitChars ls = split $ transpose ls where
    split = unfoldr go
    go [] = Nothing
    go xs = Just . second tail . break (=="   ") $ xs

analyzeGlyph :: [String] -> GlyphInfo
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

mkCharLines :: GlyphInfo -> [LineSegment]
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
