
module Haskeroids.Text where

import Haskeroids.Render
import Haskeroids.Text.Font

data Text = Text

mkText :: Font -> FontSize -> String -> Text
mkText = undefined

instance LineRenderable Text where
    interpolatedLines = undefined
