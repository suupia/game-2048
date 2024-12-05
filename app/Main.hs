module Main (main) where

import Control.Monad.State
import Control.Monad (when)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust)
import System.Random (randomRIO)
import System.Console.ANSI (Color(..), SGR(..), ConsoleLayer(..), ColorIntensity(..), setSGRCode)
import System.Exit (exitSuccess)

----------
-- Grid --
----------

type Grid = [[Int]]
type Pos = (Int, Int)

-- グリッドのサイズ
size :: Int
size = 4

-- 空のグリッドを作成する関数
emptyGrid :: Grid
emptyGrid = replicate size (replicate size 0)

-- グリッドをコンソールに描画する関数
renderGrid :: Grid -> IO ()
renderGrid grid = do
    mapM_ (putStrLn . renderRow) grid

-- グリッドを更新する関数
updateGrid :: Grid -> [(Pos, Int)] -> Grid
updateGrid = foldl updateCell
  where
    updateCell g ((r, c), val) = take r g
                            ++ [updateRow (g !! r) c val]
                            ++ drop (r + 1) g
    updateRow row c val = take c row ++ [val] ++ drop (c + 1) row

-- 一行をレンダリングするための補助関数
renderRow :: [Int] -> String
renderRow row = "|" ++ intercalate " | " (map formatTile row) ++ "|"

-- 固定幅と色で1つのタイルをフォーマットする補助関数
formatTile :: Int -> String
formatTile 0 = "    "  -- 空のセルはスペースとして表現
formatTile n = coloredString (tileColor n) (padString $ show n) 

-- タイルを揃えるためのパディングを追加
padString :: String -> String
padString s = let padding = replicate (4 - length s) ' ' in padding ++ s

-----------
-- Color --
-----------

-- 数値に対応する色を指定する関数
tileColor :: Int -> Color
tileColor n = case n of
    2    -> Yellow
    4    -> Green
    8    -> Cyan
    16   -> Blue
    32   -> Magenta
    64   -> Red
    128  -> Yellow
    256  -> Green
    512  -> Cyan
    1024 -> Blue
    2048 -> Magenta
    _    -> Red -- 2048以上のタイル用

-- 文字列に色を適用する関数
coloredString :: Color -> String -> String
coloredString color s = setSGRCode [SetColor Foreground Vivid color] ++ s ++ setSGRCode [Reset]

-----------
-- Game --
-----------

type Game a = StateT Grid IO a
data Direction = L | R | U | D

-- 入力文字列を方向に変換する関数
convertToDirection :: String -> Maybe Direction
convertToDirection "w" = Just U
convertToDirection "a" = Just L
convertToDirection "s" = Just D
convertToDirection "d" = Just R
convertToDirection _ = Nothing

-- タイルを結合し、サイズを正規化する関数
mergeTiles :: Int -> [Int] -> [Int]
mergeTiles n tiles = normalize n (merge (filter (/= 0) tiles))

-- ゼロを取り除き、長さを正規化
normalize :: Int -> [Int] -> [Int]
normalize n xs = take n (xs ++ repeat 0)

-- タイルを結合する補助関数
merge :: [Int] -> [Int]
merge [] = []
merge [x] = [x]
merge (x:y:xs)
    | x == y    = x * 2 : merge xs
    | otherwise = x : merge (y : xs)

-- 指定した方向にタイルを移動する関数
applyMove :: Direction -> Grid -> Grid
applyMove direction grid = case direction of
    L -> map (mergeTiles size) grid
    R -> map (reverse . mergeTiles size . reverse) grid
    U -> transpose $ map (mergeTiles size) (transpose grid)
    D -> transpose $ map (reverse . mergeTiles size . reverse) (transpose grid)

-- ランダムな位置にタイルを生成する関数
generateRandomTile :: Grid -> IO (Maybe Grid)
generateRandomTile grid = do
  let emptyTiles = [(r, c) | r <- [0..size-1], c <- [0..size-1], grid !! r !! c == 0]
  if null emptyTiles then return Nothing
  else do
        newTilePos <- (emptyTiles !!) <$> randomRIO (0, length emptyTiles - 1)
  　　　-- 値をランダムに選択（90%が2、10%が4）
        newTileValue <- randomRIO (1, 10 :: Int) >>= \x -> return $ if x <= 9 then 2 else 4
        -- 新しいタイルでグリッドを更新
        return $ Just $ updateGrid grid [(newTilePos, newTileValue)]

-- ゲームオーバーかどうかを判定する関数
checkGameOver :: Grid -> Bool
checkGameOver grid = all (== grid) [applyMove d grid | d <- [L, R, U, D]]

-- ゲームのメインループ 
runGame :: String -> Game ()
runGame consoleLog = do
    grid <- get

    liftIO $ do
        putStrLn "(Use WASD to move the tiles, and Q to quit.)"
        renderGrid grid
        putStrLn $ "Score: " ++ show (sum $ map sum grid)
    
    if checkGameOver grid
      then do
        liftIO $ do
            putStrLn $ coloredString Red "Game Over!"
            exitSuccess  -- プログラムを終了
      else do
        liftIO $ putStrLn consoleLog

    playerInput <- liftIO getLine

    -- Qキーでゲームを終了する 
    when (playerInput == "q") $ do
        liftIO $ do
          putStrLn "Thanks for playing!"
          exitSuccess  -- プログラムを終了

    -- WASDキーで移動する
    case convertToDirection playerInput of
        Just direction -> do
            let movedGrid = applyMove direction grid
            newGrid <- liftIO $ generateRandomTile movedGrid
            case newGrid of
                Just g -> do
                    put g
                Nothing -> return ()
        Nothing -> do
            runGame $ coloredString Yellow "無効な入力です。もう一度試してください。"
    runGame ""

-- ゲームを初期化して実行するメイン関数
main :: IO ()
main = do
    firstGrid <- fromJust <$> generateRandomTile emptyGrid 
    secondGrid <- fromJust <$> generateRandomTile firstGrid
    evalStateT (runGame "") secondGrid

