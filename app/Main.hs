module Main where

import Config
import FRP.Signal

main :: IO ()
main = initializeSDL >>= runArrow

