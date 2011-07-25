
module Haskeroids.Text.Font
    ( loadFont
    , charLines
    ) where

import Haskeroids.Geometry

import System.IO

data Font = Font
type FontSize = Float

loadFont :: FilePath -> IO Font
loadFont = fmap parseFont . readFile

parseFont :: String -> Font
parseFont s = undefined

charLines :: Font -> FontSize -> Char -> [LineSegment]
charLines = undefined

