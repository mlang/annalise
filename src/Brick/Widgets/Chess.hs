{-# LANGUAGE ViewPatterns #-}
module Brick.Widgets.Chess where

import           Brick.Types
import Data.Maybe (fromMaybe)
import           Brick.Widgets.Border
import           Brick.Widgets.Core
import           Data.Ix              (index)
import           Data.List            (intersperse)
import           Data.List.Split      (chunksOf)
import           Game.Chess

renderPosition :: n
               -> Position -> Maybe Color -> Maybe Square
               -> (Position -> Square -> Widget n)
               -> Bool
               -> Widget n
renderPosition n pos (fromMaybe (color pos) -> persp) tgt drawSq hasFocus =
  ranks <+> border board <=> files
 where
  rev :: [a] -> [a]
  rev = if persp == Black then reverse else id
  ranks = vBox (str " " : map (str . show) (rev [8 :: Int, 7..1]) <> [str " "])
  files = str $ rev "   a b c d e f g h   "
  board = hLimit 17 . vLimit 8 . vBox $ map (hBox . spacer . map pc) squares
  squares = reverse $ chunksOf 8 $ rev [A1 .. H8]
  pc sq = putCursorIf (hasFocus && tgt == Just sq) n (0,0) $
          drawSq pos sq
  spacer = (str " " :) . (<> [str " "]) . intersperse (str " ")

allPieces :: ((Color, PieceType), (Color, PieceType))
allPieces = ((Black, Pawn), (White, King))

english :: Position -> Square -> Widget n
english pos sq = case pieceAt pos sq of
  Just piece           -> str . pure $ "pnbrqkPNBRQK" !! index allPieces piece
  Nothing | isDark sq  -> str "+"
          | otherwise  -> str " "

styles :: [(String, Position -> Square -> Widget n)]
styles = [ ("English",  english)
         , ("Deutsch",  german)
         , ("Figurine", figurine)
         ]
 where
  german pos sq = case pieceAt pos sq of
    Just piece           -> str . pure $ "bsltdkBSLTDK" !! index allPieces piece
    Nothing | isDark sq  -> str "+"
            | otherwise  -> str " "
  figurine pos sq = case pieceAt pos sq of
    Just piece           -> str . pure $ "♟♞♝♜♛♚♙♘♗♖♕♔" !! index allPieces piece
    Nothing | isDark sq  -> str "+"
            | otherwise  -> str " "

putCursorIf :: Bool -> n -> (Int, Int) -> Widget n -> Widget n
putCursorIf True n loc = showCursor n $ Location loc
putCursorIf False _ _  = id
