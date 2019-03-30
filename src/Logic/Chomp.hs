-- {-# LANGUAGE MultiWayIf #-}

module Logic.Chomp where

import Control.Monad.State

-- Ex5. given positive integers m and n, interactively play the game of Chomp

-- type alias and data definition
type Position = (Int, Int)
type Moves = [Position]

poisoned = (0, 0)

-- determine whether a move is illegal
legalMove :: Int -> Int -> Moves -> Position -> Bool
legalMove m n moves pos = inBound m n pos && foldr judge True moves
  where judge prevPos acc = not (pos `beCovered` prevPos) && acc
  
inBound :: Int -> Int -> Position -> Bool
inBound m n (curX, curY) = m > curX && n > curY

beCovered :: Position -> Position -> Bool
beCovered (x, y) (prevX, prevY) = x >= prevX && y >= prevY

poisonedMove :: Position -> Bool
poisonedMove cur = cur == poisoned 

-- make some move and change global state
data GameResult = InGoing | AWin | BWin deriving Show
data Player = A | B deriving (Show, Read)
type GameStatus = (Moves, Player)

chomp :: Int -> Int -> StateT GameStatus IO GameResult
chomp m n = do
  line <- lift getLine
  let pos = read line :: Position
  (moves, player) <- get
  if legalMove m n moves pos
    then change m n pos moves player
    else do
      lift (putStrLn "Invalid Move!")
      chomp m n

change :: Int -> Int -> Position -> Moves -> Player -> StateT GameStatus IO GameResult
change m n pos moves player = do
  put (pos:moves, anotherPlayer player)  
  if poisonedMove pos 
    then return (whoWin player)
    else chomp m n 

-- helper function    
anotherPlayer :: Player -> Player
anotherPlayer A = B
anotherPlayer B = A

whoWin :: Player -> GameResult
whoWin A = BWin
whoWin B = AWin

gameStart = runStateT (chomp 4 4) ([], A)