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
getRow (b:_) 0 = b
getRow (_:bs) y = getRow bs (y - 1)
getRow [] _ = error "getRow"

setColor :: Block -> Point -> Color -> Block
setColor (b:bs) (x, 0) c = setRowColor b x c : bs
setColor (_:bs) (x, y) c = setColor bs (x, y - 1) c
setColor [] _ _ = error "setColor"

setRowColor :: Row -> X -> Color -> Row
setRowColor (_:rs) 0 c = c : rs
setRowColor (r:rs) x c = r : setRowColor rs (x - 1) c
setRowColor [] _ _ = error "setRowColor"

-- collectGroup :: Block -> [Point] -> Point -> PuyoPuyo
-- collectGroup b h (x,y) = case getColor b (x,y) of 
--                          ' ' -> []
--                          x -> getColor b (x,y) 

-- collectGroups :: Block -> [Point] -> [PuyoPuyo]

-- eraseBlock :: Block -> PuyoPuyo -> Block

-- fallBlock :: Block -> Block

