{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.HaskellEditor where

import           Brick.Types               (EventM, Widget, zoom, BrickEvent(VtyEvent))
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import           Brick.Widgets.Core        (Named (getName), txt, vBox)
import           Brick.Widgets.Edit        (handleEditorEvent, Editor, editorText, renderEditor)
import           Brick.Widgets.Skylighting (highlight)
import           Data.Binary               (Binary)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text (unlines)
import           Skylighting               (Syntax, defaultSyntaxMap,
                                            lookupSyntax)
import           Control.Lens.TH              (makeLenses)

data HaskellEditor n = HaskellEditor
  { _heEditor    :: Editor Text n
  , _heHighlight :: Bool
  }

makeLenses ''HaskellEditor

instance Named (HaskellEditor n) n where
  getName (HaskellEditor e _) = getName e

haskellEditor :: n -> Text -> HaskellEditor n
haskellEditor n t = HaskellEditor (editorText n Nothing t) True

renderHaskellEditor :: Ord n => Show n => Bool -> HaskellEditor n -> Widget n
renderHaskellEditor hasFocus (HaskellEditor e hl) =
  renderEditor (if hl then skylight else plain) hasFocus e

handleHaskellEditorEvent :: Eq n => Event -> EventM n (HaskellEditor n) ()
handleHaskellEditorEvent = zoom heEditor . handleEditorEvent . VtyEvent
  
haskell :: Syntax
haskell = fromJust $ "haskell" `lookupSyntax` defaultSyntaxMap

skylight, plain :: [Text] -> Widget n
skylight = highlight haskell . Text.unlines
plain = vBox . map txt
