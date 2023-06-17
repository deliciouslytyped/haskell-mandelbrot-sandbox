{-# LANGUAGE TemplateHaskell #-}
import Data.Complex
import Control.Monad
import Lens.Micro
import Lens.Micro.TH
import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin)

mandelbrot w zoom a b =
  let
    halfw = w / 2
    lim = 100

    endlOn p = if p then "\n" else ""
    reset = "\x1b[0m"
    bgColorSignal = "\x1b[48;2"

    iter cnt acc pos
      | (magnitude acc >= 2) = " " 
      | (cnt == lim) =
        let
          colorF v = floor $ cnt * ( abs $ (phase acc) / pi - 1 + v )
          colors = join $ fmap (\v -> ";" ++ (show $ colorF v)) [0, 1.2, 2.4]
        in
          bgColorSignal ++ colors ++ "m " ++ reset
      | otherwise = iter (cnt + 1) (acc^2 + pos) pos

    sample x y = iter 0 (0 :+ 0) $ ((x + a - w) / zoom) :+ ((y + b - halfw) / zoom)

  in
    join [ join $ [ sample x y ++ (endlOn $ (w*2-1) == x ) | x <- [0..w*2-1] ] | y <- [0..w-1]]

data MyState a = MyState { _x :: a, _y :: a, _zoom :: a } deriving Show
makeLenses ''MyState
initState :: MyState Double
initState = MyState { _x = 0, _y = 0, _zoom = 20 }

interactive _st initial =
  let
    alternateOff = "\x1b[?1049l"
    alternateOn = "\x1b[?1049h"
    hideCursor = "\x1b[?25l"
    showCursor = "\x1b[?25h"
    zeroCursor = "\x1b[0;0H"
    clear = "\x1b[2J"
    width = 40
    
    eventh s 'w' = s & y %~ subtract 5
    eventh s 's' = s & y %~ (+) 5
    eventh s 'a' = s & x %~ subtract 5
    eventh s 'd' = s & x %~ (+) 5
    -- TODO probably need to improve the numerics for deeper zooms
    eventh s '-' = ((s & zoom %~ (*) 0.90) & x %~ (*) 0.90) & y %~ (*) 0.90
    eventh s '+' = ((s & zoom %~ (*) 1.10) & x %~ (*) 1.10) & y %~ (*) 1.10
    eventh s _ = s
    
    draw st = do
      putStr $ mandelbrot width (_zoom st) (_x st) (_y st)
      putStrLn $ show $ st
  in do
    putStr $ hideCursor ++ alternateOn ++ zeroCursor
    if initial then (do
      draw _st
      putStr "\x1b[0;0H"
      ) else (pure ())
    c <- getChar
    let quit = c == 'q'
    if quit
      then (putStr $ alternateOff ++ showCursor)
      else (do 
        let st = eventh _st c
        draw st
        interactive st False
        )


main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  interactive initState True
