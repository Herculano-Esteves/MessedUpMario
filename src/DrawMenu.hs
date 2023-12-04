module DrawMenu where

import Graphics.Gloss

drawTitle :: Picture
drawTitle = Color blue $ Translate (-70) 100 $ Scale 0.3 0.3 $ text "Donkey kong"