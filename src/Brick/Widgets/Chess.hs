{-# LANGUAGE ViewPatterns #-}
module Brick.Widgets.Chess (
  InnerBorders(..), Handler, PositionRenderer
, renderPosition
, renderPosition1, renderPosition2, renderPosition3, renderPosition4
, en, de, figures, withEmptyAs, space, plus
, whitePiece, blackPiece, whiteSquare, blackSquare
, whiteToBlackSquare, blackToWhiteSquare
, putCursorIf
) where

import           Brick.AttrMap       (AttrName, attrName)
import           Brick.Types         (Location (Location), Widget)
import           Brick.Widgets.Core  (showCursor, str, withAttr)
import           Brick.Widgets.Table (ColumnAlignment (AlignCenter),
                                      RowAlignment (AlignMiddle), columnBorders,
                                      renderTable, rowBorders,
                                      setDefaultColAlignment,
                                      setDefaultRowAlignment, table)
import           Data.Ix             (index)
import           Data.List           (intersperse, mapAccumL)
import           Data.List.Split     (chunksOf)
import           Data.Maybe          (fromMaybe)
import           Game.Chess          (Color (..), PieceType (King, Pawn),
                                      Position, Square (A1, H8), color, isLight,
                                      opponent, pieceAt)

data InnerBorders = InnerBorders | NoInnerBorders deriving (Eq, Read, Show)

type Handler n = Color -> Position -> Bool -> [[Square]] -> [[Widget n]]

renderPosition :: InnerBorders -> Handler n -> Maybe Color -> Position -> Bool
               -> Widget n
renderPosition ((== InnerBorders) -> innerBorders) f pers p foc
  = renderTable
  . setDefaultColAlignment AlignCenter
  . setDefaultRowAlignment AlignMiddle
  . rowBorders innerBorders
  . columnBorders innerBorders
  . table $ f persp p foc squares
 where
  persp = fromMaybe (color p) pers
  rev | persp == White = id
      | otherwise = reverse
  squares = reverse $ chunksOf 8 $ rev [A1 .. H8]

type PositionRenderer n = n -> (Position -> Square -> Char) ->
                          Maybe Square -> Maybe Color -> Position -> Bool ->
                          Widget n

renderPosition1 :: PositionRenderer n
renderPosition1 n ch tgt = renderPosition NoInnerBorders $ \persp pos foc ->
  (map . map) (draw foc pos)
 where
  draw foc pos sq = putCursorIf (foc && tgt == Just sq) n (0, 0) $
                    attr pos sq $ str [ch pos sq]

renderPosition2 :: PositionRenderer n
renderPosition2 n char tgt = renderPosition NoInnerBorders $ \persp pos foc ->
  insertSpaces . (map . map) (draw foc pos)
 where
  draw foc pos sq = putCursorIf (foc && tgt == Just sq) n (0, 0) $
                    attr pos sq $ str [char pos sq]

renderPosition3 :: PositionRenderer n
renderPosition3 n char tgt = renderPosition NoInnerBorders $ \persp pos foc ->
  insertHalfBlocks persp . (map . map) (draw foc pos)
 where
  draw foc pos sq = putCursorIf (foc && tgt == Just sq) n (0, 0) $
                    attr pos sq $ str [char pos sq]

renderPosition4 :: PositionRenderer n
renderPosition4 n char tgt = renderPosition InnerBorders $ \persp pos foc ->
  (map . map) (draw foc pos)
 where
  draw foc pos sq = putCursorIf (foc && tgt == Just sq) n (1, 0) $
                    attr pos sq $ str [' ', char pos sq, ' ']

insertSpaces :: [[Widget n]] -> [[Widget n]]
insertSpaces = map $ (str " " :) . (<> [str " "]) . intersperse (str " ")

insertHalfBlocks :: Color -> [[Widget n]] -> [[Widget n]]
insertHalfBlocks persp = snd . mapAccumL f persp where
  f c r = (opponent c, intersperseBlocks c r)

intersperseBlocks :: Color -> [Widget n] -> [Widget n]
intersperseBlocks c xs = h c : head xs : concat ys <> [i c'] where
  (c', ys) = mapAccumL f (opponent c) (tail xs)
  f c w = (opponent c, [g c, w])
  g Black = withAttr whiteToBlackSquare $ str "▐"
  g White = withAttr blackToWhiteSquare $ str "▌"
  h Black = str "▐"
  h White = str " "
  i Black = str " "
  i White = str "▌"

allPieces :: ((Color, PieceType), (Color, PieceType))
allPieces = ((Black, Pawn), (White, King))

de, en, figures :: (Color, PieceType) -> Char
en = ("pnbrqkPNBRQK" !!) . index allPieces
de = ("bsltdkBSLTDK" !!) . index allPieces
figures = ("♟♞♝♜♛♚♙♘♗♖♕♔" !!) . index allPieces

space, plus :: Color -> Char
space _ = ' '
plus White = ' '
plus Black = '+'

withEmptyAs :: ((Color, PieceType) -> Char) -> (Color -> Char) -> Position -> Square -> Char
withEmptyAs p e pos sq = case pieceAt pos sq of
  Just piece -> p piece
  Nothing    -> e $ if isLight sq then White else Black

whitePiece, blackPiece, whiteSquare, blackSquare, whiteToBlackSquare, blackToWhiteSquare :: AttrName
whitePiece = attrName "whitePiece"
blackPiece = attrName "blackPiece"
whiteSquare = attrName "whiteSquare"
blackSquare = attrName "blackSquare"
whiteToBlackSquare = attrName "whiteToBlackSquare"
blackToWhiteSquare = attrName "blackToWhiteSquare"

attr :: Position -> Square -> Widget n -> Widget n
attr pos sq = withAttr $ s <> p where
  s = if isLight sq then whiteSquare else blackSquare
  p = case fst <$> pieceAt pos sq of
    Just White -> whitePiece
    Just Black -> blackPiece
    Nothing    -> mempty

putCursorIf :: Bool -> n -> (Int, Int) -> Widget n -> Widget n
putCursorIf True n loc = showCursor n $ Location loc
putCursorIf False _ _  = id
