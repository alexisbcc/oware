{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( playOware,
  )
where

-- | Player board consisting of 6 houses (list of 6 fixed rows) with 4 seeds each
--   for simplicity we will use indexes instead of letters
--          A | B | C | D | E | F
--          4 | 4 | 4 | 4 | 4 | 4
--          4 | 4 | 4 | 4 | 4 | 4
data Board = Board
  { -- | Player 1 house
    baseA :: [Int],
    -- | Player 2 house
    baseB :: [Int]
  }
  deriving (Show)

data Player = Player1 | Player2

createNewBoard :: Board
createNewBoard = do
  let hP1 = replicate 6 4
      hP2 = replicate 6 4
  Board
    { baseA = hP1,
      baseB = hP2
    }

playOware :: IO ()
playOware = do
  let initialBoard = createNewBoard
      scoreA = 0
      scoreB = 0
  (final, scaF, scbF) <- start initialBoard scoreA scoreB
  if scaF >= 25
    then putStrLn "Player 1 wins"
    else putStrLn "Player 2 wins"
  renderBoard final scaF scbF
  where
    start :: Board -> Int -> Int -> IO (Board, Int, Int)
    start initialBoard scoreA scoreB = do
      renderBoard initialBoard scoreA scoreB

      player1Row <- readPlayerInput "\ESC[32mPlayer 1: Choose a \ESC[34mrow"
      newBoardA <- makeAMove player1Row Player1 initialBoard
      let (scA1, scB1, harvA) = harvest Player1 newBoardA scoreA scoreB

      renderBoard harvA scA1 scB1

      player2Row <- readPlayerInput "\ESC[31mPlayer 2: Choose a \ESC[34mrow"
      newBoardB <- makeAMove player2Row Player2 harvA
      let (scA2, scB2, harvB) = harvest Player2 newBoardB scA1 scB1

      if scA2 >= 25 || scB2 >= 25
        then do
          pure (harvB, scA2, scB2)
        else start harvB scA2 scB2

    readPlayerInput :: String -> IO Int
    readPlayerInput title = do
      selected <- read <$> prompt title
      if selected > 5
        then do
          putStrLn "Please select a value between 0 and 5"
          readPlayerInput title
        else pure selected

prompt :: String -> IO String
prompt title = putStrLn title >> getLine

makeAMove :: Int -> Player -> Board -> IO Board
makeAMove row player Board {baseA, baseB} = do
  let (seeds, seedIndex) = case player of
        Player1 -> (baseA !! row, 5 - row) -- This should be "safe"
        Player2 -> (baseB !! row, 6 + row)
      baseWithIndex = zip [0 ..] $ reverse baseA ++ baseB
      startingBase = pickUpSeeds baseWithIndex seedIndex
      newBaseWithIndex = (regenBoard . reverse) $ seedHouse (seedIndex + 1) seeds startingBase []
  pure newBaseWithIndex
  where
    pickUpSeeds :: [(Int, Int)] -> Int -> [(Int, Int)]
    pickUpSeeds house startIndex = map (removeSeeds startIndex) house
    removeSeeds startIndex (ind, val) = if startIndex == ind then (ind, 0) else (ind, val)

-- | Distribure seeds across all houses
--   I'm not sure how it works, but it works
seedHouse :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
seedHouse _ 0 [] acc = acc
seedHouse ind seeds [] acc = seedHouse ind seeds acc []
seedHouse ind 0 (x : xs) acc = seedHouse ind 0 xs (x : acc)
seedHouse ind seeds ((cInd, val) : xs) acc = do
  let newInd = if ind > 11 then 0 else ind
  if newInd == cInd
    then seedHouse (newInd + 1) (seeds - 1) xs ((cInd, val + 1) : acc)
    else seedHouse newInd seeds xs ((cInd, val) : acc)

-- | From iterable full board to `Board`
regenBoard :: [(Int, Int)] -> Board
regenBoard board = do
  let (houseA, houseB) = splitAt 6 board
      bA = map snd houseA
      bB = map snd houseB
  Board {baseA = reverse bA, baseB = bB}

harvest :: Player -> Board -> Int -> Int -> (Int, Int, Board)
harvest Player1 Board {baseA, baseB} scoreA scoreB = do
  let currentScore = sum $ filter betweenF baseB
      baseBHarvested = [if betweenF val then 0 else val | val <- baseB]
      harvested = Board {baseA, baseB = baseBHarvested}
  (scoreA + currentScore, scoreB, harvested)
harvest Player2 Board {baseA, baseB} scoreA scoreB = do
  let currentScore = sum $ filter betweenF baseA
      baseAHarvested = [if betweenF val then 0 else val | val <- baseA]
      harvested = Board {baseA = baseAHarvested, baseB}
  (scoreA, scoreB + currentScore, harvested)

betweenF :: Int -> Bool
betweenF val = val >= 2 && val <= 3

-- | "Render Boards"
renderBoard :: Board -> Int -> Int -> IO ()
renderBoard Board {baseA, baseB} scoreA scoreB = do
  let ha = "\ESC[32m" <> renderHouse baseA
      hb = "\ESC[31m" <> renderHouse baseB
      scores = renderScores scoreA scoreB
  putStrLn renderLabels
  putStrLn "--------------------------------"
  putStrLn ha
  putStrLn scores
  putStrLn hb

renderLabels :: String
renderLabels = "\ESC[34m  (0)  (1)  (2)  (3)  (4)  (5)"

renderHouse :: [Int] -> String
renderHouse = foldl renderRow "   "
  where
    renderRow acc val = acc <> show val <> "    "

renderScores :: Int -> Int -> String
renderScores scoreA scoreB = ("\ESC[32m" <> show scoreA) <> "                              " <> ("\ESC[31m" <> show scoreB)
