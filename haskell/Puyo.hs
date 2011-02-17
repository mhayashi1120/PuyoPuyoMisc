module Puyo where

import Control.Applicative
import System.Console.GetOpt
import System.Environment

printUsage :: a
printUsage = error $ usageInfo usage []
  where
    usage = "\n"
         ++ "  Puyo file\n"

compilerOpts :: [String] -> String
compilerOpts argv = case getOpt Permute [] argv of
    (_, (n:_), _) -> n
    _          -> printUsage

-- main :: IO ()
-- main = do
--   file <- compilerOpts <$> getArgs
--   print file

-- loadData f = 

type Color = Char
type Row = [Color]
type Block = [Row]
type X = Int
type Y = Int
type Point = (X, Y)
type PuyoPuyo = [Point]

blocks :: Block
blocks =
    [
     "  GYRR",
     "RYYGYG",
     "GYGYRR",
     "RYGYRG",
     "YGYRYG",
     "GYRYRG",
     "YGYRYR",
     "YGYRYR",
     "YRRGRG",
     "RYGYGG",
     "GRYGYR",
     "GRYGYR",
     "GRYGYR"
    ]

getColor :: Block -> Point -> Color
getColor b (x,y) =  getRow b y !! x

getRow :: Block -> Y -> Row
getRow (_:bs) y = getRow bs (y - 1)
getRow (b:_) 0 = b
-- getRow [] _ = TODO

setColor :: Block -> Point -> Color -> Block
setColor (_:bs) (x, y) c = setColor bs (x, y - 1) c
setColor (b:bs) (x, 0) c = setRowColor b x c : bs
-- setColor [] _ _ = TODO

setRowColor :: Row -> X -> Color -> Row
setRowColor (r:rs) x c = r : setRowColor rs (x - 1) c
setRowColor (r:rs) 0 c = c : rs
-- setRowColor [] _ _ = TODO

-- collectGroup :: Block -> Point -> PuyoPuyo
-- collectGroup b p = 

-- collectGroups :: Block -> [Point] -> [PuyoPuyo]

-- eraseBlock :: Block -> PuyoPuyo -> Block

-- fallBlock :: Block -> Block

