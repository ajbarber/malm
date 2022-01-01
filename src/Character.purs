module Character where

import Data.Maybe (Maybe(..))
import P5.Image (loadImage)
import P5.Types (Image, P5)

file :: String
file = "assets/Robot/Tilesheet/character_robot_sheet.png"

characterImg :: P5 -> Image
characterImg p = loadImage p file Nothing Nothing
