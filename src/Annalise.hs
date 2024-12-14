{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Annalise (
  Config( engineExecutable
        , engineArgs
        , engineStartupTimeout
        , engineMultiPV
        , engineThreads
        , positionRenderer
        , channelSize
        , theme
        , pgnDirectory
        , polyglotBook
        , chessboardKeymap
        , configEditorKeymap
        , globalKeymap
        , onStartup
        , defaultView
        )
, sec
, ViewName(..)
, defaultConfig
, PositionRenderer
, renderPosition1, renderPosition2, renderPosition3, renderPosition4
, defaultTheme
, kate, breezeDark, pygments, espresso, tango, haddock, monochrome, zenburn
, Action
, plyInput, plyInputBS
, writeConfigFile, reloadConfigFile
, continue, relaunch, quit
, Keymap, defaultChessboardKeymap, defaultConfigEditorKeymap, defaultGlobalKeymap
, annalise
) where

import qualified Brick.AttrMap                as Brick
import           Brick.BChan                  (BChan, newBChan,
                                               writeBChanNonBlocking)
import           Brick.Focus                  (FocusRing, focusGetCurrent,
                                               focusRing, focusRingCursor,
                                               focusSetCurrent, withFocusRing)
import qualified Brick.Forms                  as Brick
import qualified Brick.Main                   as Brick
import           Brick.Types                  (BrickEvent (AppEvent, VtyEvent),
                                               EventM, Location (..),
                                               ViewportType (Both, Vertical),
                                               Widget, zoom)
import           Brick.Util                   (bg, fg, on)
import           Brick.Widgets.Border         (border, borderWithLabel)
import           Brick.Widgets.Center         (hCenter)
import           Brick.Widgets.Chess
import           Brick.Widgets.Core           (Padding (Max, Pad), emptyWidget,
                                               fill, hBox, hLimit, padLeft,
                                               padRight, padTop, showCursor,
                                               str, strWrap, txt, txtWrap, vBox,
                                               vLimit, viewport, (<+>), (<=>))
import           Brick.Widgets.Edit           (editContentsL, getEditContents,
                                               handleEditorEvent, renderEditor)
import           Brick.Widgets.FileBrowser
import qualified Brick.Widgets.FileBrowser    as Brick
import           Brick.Widgets.HaskellEditor  (handleHaskellEditorEvent,
                                               haskellEditor,
                                               renderHaskellEditor)
import qualified Brick.Widgets.List           as Brick
import           Brick.Widgets.Skylighting    (attrMappingsForStyle, highlight)
import qualified Config.Dyre                  as Dyre
import qualified Config.Dyre.Paths            as Dyre
import qualified Config.Dyre.Relaunch         as Dyre
import           Control.Concurrent           (forkIO, killThread)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Lens                 (Getter, Lens', _2, _Just,
                                               _Wrapped, assign, at, from, ix,
                                               modifying, preuse, to, use, uses,
                                               view, (%=), (%~), (&), (.=),
                                               (.~), (<>=), (<~), (?~), (^.),
                                               (^?!), (^?))
import           Control.Lens.TH              (makeLenses)
import           Control.Monad                (forever, join, void, when)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.State          (MonadState (..), get, modify)
import           Data.Bifunctor               (first, second)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import           Data.Char                    (toLower)
import           Data.FileEmbed               (embedFile)
import           Data.Foldable                (foldl', toList)
import           Data.Functor                 ((<&>))
import           Data.Generics.Labels         ()
import           Data.List                    (elemIndex, find, intercalate,
                                               intersperse, isPrefixOf, sort)
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NonEmpty
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe,
                                               isJust)
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Zipper             as Zipper
import           Data.Tree                    (Tree (..))
import           Data.Tree.NonEmpty           (breadcrumbs, listToForest)
import           Data.Tree.Zipper             (forest, fromForest, label,
                                               nextTree)
import qualified Data.Tree.Zipper             as TreePos
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import qualified Data.Vector.Unboxed          as Unboxed
import           Game.Chess
import           Game.Chess.PGN               (PGN, readPGNFile)
import qualified Game.Chess.PGN               as PGN
import           Game.Chess.Polyglot
import           Game.Chess.SAN
import qualified Game.Chess.UCI               as UCI
import qualified Graphics.Vty                 as Vty
import           Options.Applicative          hiding (str)
import           Paths_annalise               (getLibDir)
import           Skylighting                  hiding (Color)
import           System.Directory             (doesFileExist)
import           Time.Rational                (KnownDivRat)
import           Time.Units                   (sec)
import           Types

pgnGame :: Game -> PGN.Game
pgnGame g = PGN.gameFromForest [] ts PGN.Undecided where
  ts = listToForest $ g ^. #plies

------------------------------------------------------------------------------

chooseGame :: PGN -> GameChooser -> GameChooser
chooseGame pgn (GameChooser (ChooseFile fb)) = GameChooser $ ChooseGame
  (fb, Brick.list GameList (Seq.fromList $ pgn ^. _Wrapped) 1)

chooseFile :: GameChooser -> GameChooser
chooseFile (GameChooser (ChooseGame (fb, _))) = GameChooser $ ChooseFile fb

ePlies :: Getter Explorer (NonEmpty Ply)
ePlies = #treePos . to label

defaultExplorer :: PolyglotBook -> Explorer
defaultExplorer book = Explorer { .. } where
  _eInitial = startpos
  _eTreePos = fromJust . nextTree . fromForest $
              breadcrumbs <$> bookForest book _eInitial
  _eGameChooser = Nothing

data SavePGN = SavePGN
  { _spFilename :: FilePath
  , _spWhite    :: Text
  , _spBlack    :: Text
  }

makeLenses ''SavePGN

editStringField :: (Show n, Ord n)
                => Lens' s String -> n -> Maybe Int -> s -> Brick.FormFieldState s e n
editStringField l n limit =
  Brick.editField l n limit Text.pack (Just . Text.unpack . Text.intercalate "\n") (txt . Text.intercalate "\n") id

defaultTheme :: Style -> Brick.AttrMap
defaultTheme sty = Brick.attrMap Vty.defAttr $
  [ (whiteSquare <> blackPiece, fg Vty.black)
  , (blackSquare <> blackPiece, fg Vty.black)
  , (whiteSquare <> whitePiece, fg Vty.white)
  , (blackSquare <> whitePiece, fg Vty.white)
  , (blackSquare, bg Vty.green)
  , (whiteSquare, bg Vty.cyan)
  , (whiteToBlackSquare, Vty.cyan `on` Vty.green)
  , (blackToWhiteSquare, Vty.green `on` Vty.cyan)
  ] <> attrMappingsForStyle sty

defaultConfig :: Config
defaultConfig = Config { .. } where
  engineExecutable = "stockfish"
  engineArgs = []
  engineStartupTimeout = sec 5
  engineMultiPV = Nothing
  engineThreads = Nothing
  positionRenderer = renderPosition2
  channelSize = 20
  theme = defaultTheme breezeDark
  pgnDirectory = Nothing
  polyglotBook = Nothing
  helpKeymap = defaultHelpKeymap
  chessboardKeymap = defaultChessboardKeymap
  explorerKeymap = defaultExplorerKeymap
  pgnBrowserKeymap = defaultPgnBrowserKeymap
  gameListKeymap = defaultGameListKeymap
  configEditorKeymap = defaultConfigEditorKeymap
  globalKeymap = defaultGlobalKeymap
  onStartup = pure ()
  defaultView = ChessboardView
  configFile = undefined
  dyreError = Nothing

commandLine :: Config -> Parser Config
commandLine cfg = Config <$>
  strOption (long "engine-executable"
          <> metavar "NAME"
          <> showDefault <> value (cfg ^. #engineExecutable)
          <> help "Path to the UCI engine executable") <*>
  pure (cfg ^. #engineArgs) <*>
  pure (cfg ^. #engineStartupTimeout) <*>
  optional (option auto $ long "engine-multi-pv" <> metavar "N" <> case cfg ^. #engineMultiPV of {Just i -> value i <> showDefault; Nothing -> mempty}) <*>
  optional (option auto $ long "engine-threads" <> metavar "N" <> case cfg ^. #engineThreads of {Just i -> value i <> showDefault; Nothing -> mempty}) <*>
  pure (cfg ^. #positionRenderer) <*>
  option auto (long "channel-size"
            <> metavar "INT"
            <> help "Event channel size"
            <> showDefault <> value (cfg ^. #channelSize)) <*>
  pure (cfg ^. #theme) <*>
  optional (strOption (long "pgn-directory" <> metavar "DIR" <> case cfg ^. #pgnDirectory of { Just dir -> showDefault <> value dir; Nothing -> mempty })) <*>
  optional (strOption (long "polyglot-book" <> metavar "DIR" <> case cfg ^. #polyglotBook of { Just dir -> showDefault <> value dir; Nothing -> mempty })) <*>
  pure (cfg ^. #helpKeymap) <*>
  pure (cfg ^. #chessboardKeymap) <*>
  pure (cfg ^. #explorerKeymap) <*>
  pure (cfg ^. #pgnBrowserKeymap) <*>
  pure (cfg ^. #gameListKeymap) <*>
  pure (cfg ^. #configEditorKeymap) <*>
  pure (cfg ^. #globalKeymap) <*>
  pure (cfg ^. #onStartup) <*>
  pure (cfg ^. #defaultView) <*>
  pure (cfg ^. #configFile) <*>
  pure (cfg ^. #dyreError)

withFocus :: (ViewName -> Name -> Action a) -> Action a
withFocus f = do
  vn <- fromJust . focusGetCurrent <$> use #focus
  n <- fromJust . focusGetCurrent . fromJust <$> use (#viewFocus . at vn)
  f vn n

newPGNBrowser :: Action (FileBrowser Name)
newPGNBrowser = use (#config . #pgnDirectory) >>= \dir ->
  liftIO (newFileBrowser selectNonDirectories FileBrowser dir) <&>
  setFileBrowserEntryFilter (Just $ fileExtensionMatch "pgn")

togglePerspective :: Action ()
togglePerspective = withFocus go where
  go ChessboardView _ = do
    modifying (#game . #perspective) $ \case
      Nothing    -> Just White
      Just White -> Just Black
      Just Black -> Nothing

newGameChooser :: Action GameChooser
newGameChooser = GameChooser . ChooseFile <$> newPGNBrowser

------------------------------------------------------------------------------

mkAnalyser :: MonadIO m => Config -> m (Maybe Analyser)
mkAnalyser Config{..} =
  liftIO (UCI.start' engineStartupTimeout (const $ pure ()) engineExecutable engineArgs) >>= \case
    Nothing -> pure Nothing
    Just e -> do
      let maybeSet o = maybe (pure ()) $ \v ->
            void $ UCI.setOptionSpinButton o v e
      maybeSet "MultiPV" engineMultiPV
      maybeSet "Threads" engineThreads
      pure . Just $
        Analyser e Nothing Vector.empty

renderHelp :: AppState -> [Widget Name]
renderHelp s = [ui] where
  ui = header <=> vp where
    header = hCenter . border . padLeft (Pad 3) . padRight (Pad 3) $
             txt "annalise help"
    vp = viewport Help Vertical content
    content = vBox $ padTop (Pad 1) . r <$>
      [ s ^. #config . #globalKeymap
      , s ^. #config . #chessboardKeymap
      , s ^. #config . #explorerKeymap
      , s ^. #config . #configEditorKeymap
      ]
    r = Map.foldlWithKey f emptyWidget where
      f w k b = w <=> ((vLimit 1 . hLimit 20 $ txt (ppVtyEvent k) <+> fill ' ') <+> txtWrap (b ^. #description))

ppVtyEvent :: Vty.Event -> Text
ppVtyEvent (Vty.EvKey k mods) = Text.intercalate "-" $
  fmap ppMod mods <> [ppKey k]

ppKey :: Vty.Key -> Text
ppKey (Vty.KChar '\t') = "<Tab>"
ppKey (Vty.KChar c)    = Text.singleton c
ppKey Vty.KRight       = "<Right>"
ppKey Vty.KLeft        = "<Left>"
ppKey Vty.KUp          = "<Up>"
ppKey Vty.KDown        = "<Down>"
ppKey Vty.KEnter       = "<Enter>"
ppKey Vty.KBS          = "<Backspace>"
ppKey Vty.KEsc         = "<Escape>"
ppKey Vty.KDel         = "<Delete>"
ppKey Vty.KHome        = "<Home>"
ppKey Vty.KEnd         = "<End>"
ppKey (Vty.KFun n)     = Text.pack $ "<F" <> show n <> ">"

ppMod :: Vty.Modifier -> Text
ppMod Vty.MMeta = "M"
ppMod Vty.MCtrl = "C"

helpUp, helpDown :: Action ()
helpUp = vScrollBy Help (-1)
helpDown = vScrollBy Help 1

defaultHelpKeymap :: Keymap
defaultHelpKeymap = Map.fromList
  [ ( Vty.EvKey Vty.KUp []
    , simpleBinding "Scroll up" $ helpUp *> continue )
  , ( Vty.EvKey Vty.KDown []
    , simpleBinding "Scroll down" $ helpDown *> continue )
  ]

helpHandler :: EventHandler
helpHandler = EventHandler (#config . #helpKeymap) $ const continue

------------------------------------------------------------------------------

eCurrentPosition, ePreviousPosition :: Explorer -> Position
eCurrentPosition e = foldl' unsafeDoPly (e ^. #initial) (e^.ePlies)
ePreviousPosition e = foldl' unsafeDoPly (e ^. #initial) (e^.ePlies.to NonEmpty.init)

elemList :: Eq a => n -> a -> [a] -> Brick.List n a
elemList n x xs = Brick.list n (Vector.fromList xs) 1
                & Brick.listSelectedL .~ i where
  i = x `elemIndex` xs

ePlyList :: Explorer -> Brick.List Name Ply
ePlyList (view #treePos -> tp) = elemList PlyList ply plies where
  ply = NonEmpty.last . label $ tp
  plies = fmap (NonEmpty.last . rootLabel) . forest $ tp

renderExplorer :: AppState -> [Widget Name]
renderExplorer s = go $ view #explorer s where
  go e = fb <> [ui] where
    ui = (hLimit 9 list <+> board <+> var)
     <=> fill ' '
     <=> renderMessage s
    list = withFocusRing' s ExplorerView
      (Brick.renderList (drawPly (ePreviousPosition e))) (ePlyList e)
    drawPly p foc = putCursorIf foc PlyList (0, 0) . str . toSAN p
    board = (s ^. #config . #positionRenderer) Chessboard (en `withEmptyAs` space) Nothing (Just (color (ePreviousPosition e))) (eCurrentPosition e) True
    var = strWrap $ varToSAN (e ^. #initial) (e ^. #treePos . to label)
    fb = case e ^. #gameChooser of
      Just (GameChooser (ChooseFile fb)) ->
        [renderFileBrowser True fb <=> renderMessage s]
      Just (GameChooser (ChooseGame (_, l))) ->
        [Brick.renderList drawSummary True l]
      Nothing -> []
    drawSummary foc = putCursorIf foc GameList (0, 0) . txt . pgnGameSummary

pgnGameSummary :: PGN.Game -> Text
pgnGameSummary g =
  let tags = g ^. PGN.cgTags
      tag = (`lookup` tags)
      prepend (tag -> x) (tag -> y) =
        (x >>= (<$> y) . Text.append . flip Text.snoc ' ') <|> y
      o = case g ^. PGN.cgOutcome of
            PGN.Win White -> "1-0"
            PGN.Win Black -> "0-1"
            PGN.Draw      -> "½-½"
            PGN.Undecided -> "*"
  in Text.intercalate " - " $ catMaybes
     [ tag "Event" <|> tag "Site" <|> tag "Date"
     , prepend "WhiteTitle" "White"
     , prepend "BlackTitle" "Black"
     ] <> [o]

withFocusRing' s v f a = withFocusRing (s ^?! #viewFocus . ix v)
  f a

explorerHandler :: EventHandler
explorerHandler = EventHandler (#config . #explorerKeymap) $ \e -> do
  message $ UnboundEvent e
  continue


explorerPrev, explorerNext, explorerFirstChild, explorerParent :: Action ()
explorerPrev = #explorer . #treePos %= (fromMaybe <*> TreePos.prev)
explorerNext = #explorer . #treePos %= (fromMaybe <*> TreePos.next)
explorerFirstChild = #explorer . #treePos %= (fromMaybe <*> TreePos.firstChild)
explorerParent = #explorer . #treePos %= (fromMaybe <*> TreePos.parent)

createGameFromExplorer :: Action ()
createGameFromExplorer = do
  pos <- use $ #explorer . #initial
  plies <- use $ #explorer . ePlies
  changeGame $ do
    #game . #initial .= pos
    #game . #plies .= toList plies
    sideToMove <- color <$> uses #game currentPosition
    #game . #cursor .= (if sideToMove == White then E1 else E8)
  switchView ChessboardView

setViewFocus :: ViewName -> Name -> Action ()
setViewFocus vn n = #viewFocus . ix vn %= focusSetCurrent n

eOpenFile :: Action ()
eOpenFile = do
  #explorer . #gameChooser <~ Just <$> newGameChooser
  setViewFocus ExplorerView FileBrowser

abort :: Action ()
abort = withFocus go where
  go ExplorerView FileBrowser = eAbortOpenFile
  go ExplorerView GameList = do
    #explorer . #gameChooser . _Just %= chooseFile
    setViewFocus ExplorerView FileBrowser
  go _ _                     = pure ()

eAbortOpenFile :: Action ()
eAbortOpenFile = do
  #explorer . #gameChooser .= Nothing
  setViewFocus ExplorerView PlyList

defaultExplorerKeymap = Map.fromList
  [ ( Vty.EvKey Vty.KDown []
    , simpleBinding "Select next move" $ explorerNext *> continue
    )
  , ( Vty.EvKey Vty.KUp []
    , simpleBinding "Select previous move" $ explorerPrev *> continue
    )
  , ( Vty.EvKey Vty.KRight []
    , simpleBinding "First child" $ explorerFirstChild *> continue
    )
  , ( Vty.EvKey Vty.KLeft []
    , simpleBinding "Parent" $ explorerParent *> continue
    )
  , ( Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]
    , simpleBinding "Create game from this variation" $ createGameFromExplorer *> continue
    )
  , ( Vty.EvKey (Vty.KChar 'o') [Vty.MCtrl]
    , simpleBinding "Open PGN file" $ eOpenFile *> continue
    )
  ]

------------------------------------------------------------------------------

pgnBrowserEnter :: Action ()
pgnBrowserEnter = use (#explorer . #gameChooser) >>= \case
  Just (GameChooser (ChooseFile fb)) -> case Brick.fileBrowserCursor fb of
    Just fileInfo -> readPGNFile (fileInfoFilePath fileInfo) >>= \case
      Right pgn -> do
        #explorer . #gameChooser . _Just %= chooseGame pgn
        setViewFocus ExplorerView GameList
      Left err -> message err
    Nothing -> pure ()
  Just (GameChooser (ChooseGame (_, l))) -> case snd <$> Brick.listSelectedElement l of
    Just game -> do
      let forest = breadcrumbs . fmap (view PGN.annPly) <$> game ^. PGN.cgForest
      case nextTree (fromForest forest) of
        Just treePos -> do
          #explorer . #treePos .= treePos
          eAbortOpenFile
        Nothing -> message ("Empty game" :: String)
    Nothing -> pure ()

doesPGNExist :: Action Bool
doesPGNExist = do
  fb <- preuse $ #explorer . #gameChooser . _Just . #step . _ChooseFile
  maybe (pure False) (liftIO . doesFileExist . fileInfoFilePath) $
    fileBrowserCursor =<< fb

pgnBrowserIsSearching :: Action Bool
pgnBrowserIsSearching =
  preuse (#explorer . #gameChooser . _Just . #step . _ChooseFile) <&>
  maybe False fileBrowserIsSearching

pgnBrowserHandler :: EventHandler
pgnBrowserHandler = EventHandler (#config . #pgnBrowserKeymap) $ \e -> do
  zoom (#explorer . #gameChooser . _Just . #step . _ChooseFile) $
    handleFileBrowserEvent e


defaultPgnBrowserKeymap :: Keymap
defaultPgnBrowserKeymap = Map.fromList
  [ ( Vty.EvKey Vty.KEnter []
    , Binding "Select file" doesPGNExist $ pgnBrowserEnter *> continue
    )
  , ( Vty.EvKey Vty.KEsc []
    , Binding "Abort" (not <$> pgnBrowserIsSearching) $
      abort *> continue
    )
  ]

defaultGameListKeymap :: Keymap
defaultGameListKeymap = Map.fromList
  [ ( Vty.EvKey Vty.KEnter []
    , simpleBinding "Open Game" $ pgnBrowserEnter *> continue
    )
  , ( Vty.EvKey Vty.KEsc []
    , simpleBinding "Go back to file list" $ abort *> continue
    )
  ]

gameListHandler :: EventHandler
gameListHandler = EventHandler (#config . #gameListKeymap) $ \e -> do
  zoom (#explorer . #gameChooser . _Just . #step . _ChooseGame . _2) $
    Brick.handleListEvent e

------------------------------------------------------------------------------

toggleAnalyser :: Action ()
toggleAnalyser = do
  use #analyser >>= \case
    Nothing -> startAnalyser
    Just a -> do
      stopSearch
      liftIO $ UCI.quit (a ^. #engine)
      #analyser .= Nothing

startAnalyser :: MonadIO m => MonadState AppState m => m ()
startAnalyser = do
  use #config >>= mkAnalyser >>= \case
    Nothing -> message ("Failed to start engine" :: String)
    Just a -> do
      #analyser .= Just a
      startSearch

startSearch :: MonadIO m => MonadState AppState m => m ()
startSearch = do
  use #analyser >>= \case
    Just a -> do
      pos <- use $ #game . #initial
      UCI.setPosition (a ^. #engine) pos =<< use (#game . #plies)
      (bmc, ic) <- UCI.search (a ^. #engine) [UCI.infinite]
      chan <- use #channel
      tid <- liftIO . forkIO . forever $
        atomically (readTChan ic) >>= writeBChanNonBlocking chan
      #analyser .= Just (a & #reader ?~ (bmc, tid))
    Nothing -> pure ()

stopSearch :: MonadIO m => MonadState AppState m => m Bool
stopSearch = do
  use #analyser >>= \case
    Just a -> case a ^. #reader of
      Just (bmc, tid) -> do
        liftIO $ do
          killThread tid
          let e = a ^. #engine
          UCI.stop e
          void . atomically $ readTChan bmc
          UCI.isready e
        #analyser . traverse . #reader .= Nothing
        #analyser . traverse . #pvs     .= Vector.empty
        pure True
      Nothing -> pure False
    Nothing -> pure False

changeGame :: MonadIO m => MonadState AppState m => m a -> m a
changeGame action = do
  stopped <- stopSearch
  a <- action
  when stopped $ startSearch
  pure a

------------------------------------------------------------------------------

haskellSyntax :: Syntax
haskellSyntax = fromJust $ "haskell" `lookupSyntax` defaultSyntaxMap

renderView :: ViewName -> AppState -> [Widget Name]
renderView HelpView         = renderHelp
renderView ChessboardView   = renderGame
renderView ExplorerView     = renderExplorer
renderView ConfigEditorView = renderConfigEditor

renderConfigEditor :: AppState -> [Widget Name]
renderConfigEditor s = [vBox $ edit <> err <> msg <> stat] where
  edit = [renderHaskellEditor True $ s ^. #configEditor]
  err = case s ^. #config . #dyreError of
    Nothing -> []
    Just e -> [borderWithLabel (str "Rebuild error") $
               viewport RebuildError Both $
               str e]
  msg = [renderMessage s]
  stat = [vLimit 1 $ str "C-q to quit"]

renderMessage :: AppState -> Widget n
renderMessage s = maybe (str "") txtWrap $ s ^. #message

configEditorText :: Getter AppState Text
configEditorText = #configEditor . #editor . to getEditContents . to Text.unlines

defaultGlobalKeymap :: Keymap
defaultGlobalKeymap = Map.fromList
  [ ( Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]
    , simpleBinding "Quit the application" quit
    )
  , ( Vty.EvKey (Vty.KFun 1) []
    , simpleBinding "Help view" $ switchView HelpView *> continue
    )
  , ( Vty.EvKey (Vty.KFun 2) []
    , simpleBinding "Game view" $ switchView ChessboardView *> continue
    )
  , ( Vty.EvKey (Vty.KFun 3) []
    , simpleBinding "Book explorer" $ switchView ExplorerView *> continue
    )
  , ( Vty.EvKey (Vty.KFun 10) []
    , simpleBinding "Configuration editor" $
      switchView ConfigEditorView *> continue
    )
  ]

switchView :: ViewName -> Action ()
switchView = modify . setView

clearMessage :: Action ()
clearMessage = #message .= Nothing

class Message a where
  message :: MonadState AppState m => a -> m ()

instance Message Text where
  message = assign #message . Just

instance Message String where
  message = message . Text.pack

instance Message Info where
  message (UnboundEvent e) = message $ "Unbound event: " <> ppVtyEvent e
  message (WroteFile fp)   = message $ "Wrote " <> fp
  message (SeveralPiecesCanMoveHere sq pos pls) = message $
    "Several pieces can move to " <> show sq <> ": " <> intercalate "|" (fmap (toSAN pos) pls)
  message (NotAllowedToMove c p sq) = message $
    "You are not allowed to move the " <> show c <> " " <> show p <> " at " <> show sq

writeConfigFile :: Action ()
writeConfigFile = do
  fp <- use $ #config . #configFile
  liftIO . ByteString.writeFile fp . Text.encodeUtf8 =<< use configEditorText
  message $ WroteFile fp

reloadConfigFile :: MonadIO m => MonadState AppState m => Bool -> m ()
reloadConfigFile verbose = do
  fp <- use $ #config . #configFile
  exists <- liftIO $ doesFileExist fp
  a <- Text.decodeUtf8 <$>
       if | exists    -> liftIO $ ByteString.readFile fp
          | otherwise -> pure $(embedFile "app/Main.hs")
  #configEditor .= haskellEditor ConfigEditor a
  when (verbose && exists) $
    message $ "Config loaded from " <> fp

loadBook :: MonadIO m => MonadState AppState m => m ()
loadBook = do
  use (#config . #polyglotBook) >>= \case
    Nothing -> pure ()
    Just fp -> do
      exists <- liftIO $ doesFileExist fp
      book <- liftIO $ readPolyglotFile fp
      #explorer . #treePos .= (fromJust . nextTree . fromForest)
        (breadcrumbs <$> bookForest book startpos)
      pure ()

gotoBeginningOfConfig, gotoEndOfConfig :: Action ()
gotoBeginningOfConfig = modifying (#configEditor . #editor . editContentsL) Zipper.gotoBOF
gotoEndOfConfig       = modifying (#configEditor . #editor . editContentsL) Zipper.gotoEOF

vScrollBy :: Name -> Int -> Action ()
vScrollBy n = Brick.vScrollBy (Brick.viewportScroll n)

scrollRebuildErrorUp, scrollRebuildErrorDown :: Action ()
scrollRebuildErrorUp = vScrollBy RebuildError (-1)
scrollRebuildErrorDown = vScrollBy RebuildError 1

continue :: Action ()
continue = return ()

relaunch :: Action ()
relaunch = do
  persist
  #relaunch .= True
  quit

quit :: Action ()
quit = Brick.halt

defaultConfigEditorKeymap :: Keymap
defaultConfigEditorKeymap = Map.fromList
  [ ( Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]
    , simpleBinding "Save and relaunch" $
      writeConfigFile *> relaunch
    )
  , ( Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]
    , simpleBinding "Write configuration to disk" $
      writeConfigFile *> continue
    )
  , ( Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]
    , simpleBinding "Revert from disk or default" $
      reloadConfigFile True *> continue
    )
  , ( Vty.EvKey (Vty.KChar '<') [Vty.MMeta]
    , simpleBinding "Go to beginning of configuration" $
      gotoBeginningOfConfig *> continue
    )
  , ( Vty.EvKey (Vty.KChar '>') [Vty.MMeta]
    , simpleBinding "Go to end of configuration" $
      gotoEndOfConfig *> continue
    )
  , ( Vty.EvKey (Vty.KChar 'n') [Vty.MMeta]
    , simpleBinding "Scroll error down" $
      scrollRebuildErrorDown *> continue
    )
  , ( Vty.EvKey (Vty.KChar 'p') [Vty.MMeta]
    , simpleBinding "Scroll error up" $
      scrollRebuildErrorUp *> continue
    )
  ]

data EventHandler = EventHandler
  (Getter AppState Keymap)
  (Vty.Event -> Action ())

configEditorHandler :: EventHandler
configEditorHandler = EventHandler (#config . #configEditorKeymap) $ \e -> do
  zoom #configEditor $ handleHaskellEditorEvent e

defaultChessboardKeymap :: Keymap
defaultChessboardKeymap = Map.fromList $
  ([ ( Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl]
     , simpleBinding "Toggle analyser" $ toggleAnalyser *> continue
     )
   , ( Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]
     , simpleBinding "Toggle analyser" $ togglePerspective *> continue
     )
   , ( Vty.EvKey Vty.KLeft []
     , simpleBinding "Move one square to the left" $ cursorLeft *> continue
     )
   , ( Vty.EvKey Vty.KRight []
     , simpleBinding "Move one square to the right" $ cursorRight *> continue
     )
   , ( Vty.EvKey Vty.KUp []
     , simpleBinding "Move one square up" $ cursorUp *> continue
     )
   , ( Vty.EvKey Vty.KDown []
     , simpleBinding "Move one square down" $ cursorDown *> continue
     )
   , ( Vty.EvKey Vty.KEnter []
     , simpleBinding "Select current square" $ enter *> continue
     )
   , ( Vty.EvKey Vty.KBS []
     , simpleBinding "Delete ply input backwards" $ plyInputBS *> continue
     )
   , ( Vty.EvKey Vty.KEsc []
     , simpleBinding "Clear ply input" $ clearInput *> continue
     )
   , ( Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]
     , simpleBinding "Reset game" $ resetGame *> continue
     )
   , ( Vty.EvKey Vty.KDel []
     , simpleBinding "Takeback" $ takeback *> continue
     )
   ] <>
  ) $
  "abcdefgh12345678knqrxo" <&> \c ->
  ( Vty.EvKey (Vty.KChar c) []
  , simpleBinding "Ply input" $ plyInput c *> continue
  )

chessboardHandler :: EventHandler
chessboardHandler = EventHandler (#config . #chessboardKeymap) $ \e -> do
  message $ UnboundEvent e
  continue

resetGame :: Action ()
resetGame = changeGame $ #game .= defaultGame

clearPlyInput :: Action ()
clearPlyInput = #game . #input .= ""

plyInput :: Char -> Action ()
plyInput c = do
  ok <- uses #game (validPlyInput c)
  if ok then do
    #game . #input <>= [c]
    uses #game selectedPly >>= \case
      Just ply -> do
        clearPlyInput
        addPly ply
      Nothing -> do
        input <- use $ #game . #input
        #game . #input <~ plyPrefix input <$> uses #game currentPosition
  else use (#game . #input) >>= \case
    "" -> message $ "No ply starts with '" <> Text.singleton c <> "'"
    _  -> message $ "No ply continues with '" <> Text.singleton c <> "'"

validPlyInput :: Char -> Game -> Bool
validPlyInput c g = not . null $
  selectedPlies (currentPosition g) (g ^. #input <> [c])

plyInputBS :: Action ()
plyInputBS = modifying (#game . #input) $ \case
  "" -> ""
  s  -> init s

selectedPly :: Game -> Maybe Ply
selectedPly g = case g ^. #input of
  "" -> Nothing
  str -> case selectedPlies (currentPosition g) str of
    [ply] -> Just ply
    _ -> Nothing

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix _ []                      = []
commonPrefix [] _                      = []
commonPrefix (x:xs) (y:ys) | x == y    = x:commonPrefix xs ys
                           | otherwise = []

plyPrefix :: String -> Position -> String
plyPrefix input pos = case selectedPlies pos input of
  [] -> ""
  xs -> foldl1 commonPrefix $ map toLower . toSAN pos <$> xs

selectedPlies :: Position -> String -> [Ply]
selectedPlies pos (map toLower -> input) =
  filter ((input `isPrefixOf`) . filter (/= '-') . map toLower . toSAN pos) $
  legalPlies pos

drawSelectedPlies :: Position -> String -> Widget n
drawSelectedPlies pos input = hBox . intersperse (str " ") $ choices where
  choices = str <$> sort (toSAN pos <$> selectedPlies pos input)
  -- maybeStripPrefix xs ys = fromMaybe ys $ stripPrefix xs ys

takeback :: Action ()
takeback = changeGame . modifying (#game . #plies) $ \case
  [] -> []
  xs -> init xs

handleViewEvent :: ViewName -> Name -> Vty.Event -> Action ()
handleViewEvent = go where
  go HelpView         _       = dispatch helpHandler
  go ChessboardView   _       = dispatch chessboardHandler
  go ExplorerView     PlyList = dispatch explorerHandler
  go ExplorerView FileBrowser = dispatch pgnBrowserHandler
  go ExplorerView GameList    = dispatch gameListHandler
  go ConfigEditorView _       = dispatch configEditorHandler
  dispatch (EventHandler keymapL fallback) e = get >>= action where
    local s = case Map.lookup e (s ^. keymapL) of
      Just b -> do
        g <- b ^. #guard
        if g then b ^. #action else fallback e
      Nothing -> fallback e
    action s = do
      clearMessage
      case Map.lookup e (s ^. #config . #globalKeymap) of
        Just b -> do
          g <- b ^. #guard
          if g then b ^. #action else local s
        Nothing -> local s

------------------------------------------------------------------------------

currentPosition, previousPosition :: Game -> Position
currentPosition g = foldl' unsafeDoPly (g ^. #initial) (g ^. #plies)
previousPosition g = foldl' unsafeDoPly (g ^. #initial) (init (g ^. #plies))

cursorMod :: (Bool -> (Rank, File) -> (Rank, File)) -> Action ()
cursorMod f = do
  normal <- (== White) <$> uses #game (color . currentPosition)
  modifying (#game . #cursor) $ \sq ->
    f normal (sq ^. rankFile) ^. from rankFile

safeSucc, safePred :: (Bounded a, Enum a, Eq a) => a -> a
safeSucc n | n == maxBound = n
           | otherwise     = succ n
safePred n | n == minBound = n
           | otherwise     = pred n

cursorLeft, cursorRight, cursorUp, cursorDown :: Action ()
cursorLeft  = cursorMod $ \normal -> second $ if normal then safePred else safeSucc
cursorRight = cursorMod $ \normal -> second $ if normal then safeSucc else safePred
cursorUp    = cursorMod $ \normal -> first  $ if normal then safeSucc else safePred
cursorDown  = cursorMod $ \normal -> first  $ if normal then safePred else safeSucc

clearInput :: Action ()
clearInput = do
  #game . #input .= ""
  #game . #from .= Nothing

enter :: Action ()
enter = do
  ok <- uses (#game . #input) null
  when ok $ do
    use (#game . #from) >>= \case
      Nothing -> do
        sq <- use $ #game . #cursor
        let goesTo pl = plyTarget pl == sq
        pos <- uses #game currentPosition
        let plies = legalPlies pos
        case filter goesTo plies of
          [] -> do
            let comesFrom pl = plySource pl == sq
            case filter comesFrom plies of
              [] -> case pieceAt pos sq of
                Nothing     -> message $ "No piece can go to " <> show sq
                Just (c, p) -> message $ NotAllowedToMove c p sq
              [pl] -> addPly pl
              xs -> assign (#game . #from) $ Just sq
          [pl] -> addPly pl
          xs -> message $ SeveralPiecesCanMoveHere sq pos xs
      Just src -> use (#game . #cursor) >>= \dst -> do
        let p pl = plySource pl == src && plyTarget pl == dst
        plies <- legalPlies <$> uses #game currentPosition
        case filter p plies of
          [] -> do
            message $ show dst <> " is not a valid target square"
          [pl] -> addPly pl
          _ -> pure ()

addPly :: Ply -> Action ()
addPly pl = changeGame $ do
  #game . #plies <>= [pl]
  #game . #from .= Nothing
  #game . #input .= ""

renderGame :: AppState -> [Widget Name]
renderGame s = [w $ s ^. #game] where
  status = renderMessage s
  pv = case s ^? #analyser . traverse . #pvs of
    Just pvs -> let pos = s ^. #game . to currentPosition
                in vBox $ drawPV pos <$> toList pvs
    _              -> emptyWidget
  drawPV pos (PV s b pv) = hLimit 10 (padLeft Max (drawScore s)) <+> str " " <+> str (varToSAN pos pv)
  drawScore = \case
    UCI.CentiPawns s -> str . show $ realToFrac s / 100
    UCI.MateIn hm | hm >= 0 -> str $ "#" <> show hm
                  | otherwise -> str $ "#" <> show hm

  w g = (board <+> var)
    <=> (hLimit 21 . vLimit 1 $ sideToMove <+> fill ' ' <+> lastPly)
    <=> input
    <=> pv
    <=> fill ' '
    <=> status
   where
    board = (s ^. #config . #positionRenderer) Chessboard (en `withEmptyAs` space) cursor (g ^. #perspective) pos (null $ g ^. #input)
    cursor | null (g ^. #input) = Just $ g ^. #cursor
           | otherwise          = Nothing
    var = strWrap . varToSAN (g ^. #initial) $ g ^. #plies
    sideToMove = str . show . color $ pos
    lastPly = case g ^. #plies of
      [] -> str "START"
      ps -> str (show (moveNumber pos))
        <+> str (if color pos == Black then "." else "...")
        <+> str (toSAN (previousPosition g) (last ps))
    input = case g ^. #input of
      "" -> emptyWidget
      input -> showCursor Chessboard (Location (length input, 0)) (str input)
           <+> str "{" <+> plies <+> str "}"
    plies = drawSelectedPlies pos (g ^. #input)
    pos = currentPosition g

------------------------------------------------------------------------------

initialState :: BChan AppEvent -> Config -> AppState
initialState chan cfg =
  AppState chan cfg focus viewFocus defaultGame Nothing (defaultExplorer defaultBook) edit Nothing False
 where
  focus = focusSetCurrent (cfg ^. #defaultView) (focusRing [minBound .. maxBound])
  viewFocus = Map.fromList
    [ ( ChessboardView, focusRing [Chessboard] )
    , ( ExplorerView, focusRing [PlyList, FileBrowser, GameList] )
    , ( ConfigEditorView, focusRing [ConfigEditor] )
    ]
  edit = haskellEditor ConfigEditor (Text.decodeUtf8 $(embedFile "app/Main.hs"))

focusedView :: Getter AppState (Maybe ViewName)
focusedView = #focus . to focusGetCurrent

setView :: ViewName -> AppState -> AppState
setView v = #focus %~ focusSetCurrent v

viewName :: AppState -> ViewName
viewName s = fromMaybe (s ^. #config . #defaultView) $ s ^. focusedView

focusedWidget :: AppState -> Maybe (FocusRing Name)
focusedWidget s = s ^. #viewFocus . at (viewName s)

addPV :: Int -> UCI.Score -> Maybe UCI.Bounds -> Unboxed.Vector Ply
      -> Vector PredictedVariation -> Vector PredictedVariation
addPV i score bounds pv pvs
  | i == Vector.length pvs
  = pvs <> Vector.singleton (PV score bounds pv)
  | otherwise
  = pvs Vector.// [(i, PV score bounds pv)]

app :: Brick.App AppState AppEvent Name
app = Brick.App { .. } where
  appStartEvent = do
    reloadConfigFile False
    loadBook
    restore
    join . use $ #config . #onStartup
  appDraw s = renderView (viewName s) s
  appHandleEvent = go where
    go (VtyEvent e) = do
      vn <- viewName <$> get
      s <- get
      let n = fromJust . focusGetCurrent $ s ^?! #viewFocus . ix vn
      handleViewEvent vn n e
    go (AppEvent i) = case (find isMultiPV i, find isScore i, find isPV i) of
      (Just (UCI.MultiPV i'), Just (UCI.Score score bounds), Just (UCI.PV pv)) ->
        modifying (#analyser . traverse . #pvs) $ addPV (pred i') score bounds pv
      _ -> pure ()
    go _            = pure ()
  appAttrMap = view $ #config . #theme
  appChooseCursor s
    | isJust $ s ^? #viewFocus . ix (viewName s)
    = focusRingCursor (^?! #viewFocus . ix (viewName s)) s
    | otherwise
    = Brick.neverShowCursor s

isScore, isPV, isMultiPV :: UCI.Info -> Bool
isScore UCI.Score{} = True
isScore _           = False
isPV UCI.PV{} = True
isPV _        = False
isMultiPV UCI.MultiPV{} = True
isMultiPV _             = False

------------------------------------------------------------------------------

persistent :: MonadState AppState m => m Persistent
persistent =
  Persistent <$> use (#focus . to focusGetCurrent . to fromJust)
             <*> use #game

persist, restore :: Action ()
persist = liftIO . Dyre.saveBinaryState =<< persistent
restore = do
  (Persistent vn g) <- liftIO . Dyre.restoreBinaryState =<< persistent
  #focus %= focusSetCurrent vn
  #game .= g

------------------------------------------------------------------------------

main :: Config -> IO ()
main inCfg = do
  cfg <- execParser $ info (commandLine inCfg <**> helper) $ briefDesc
  chan <- newBChan $ cfg ^. #channelSize
  (appState, _) <- Brick.customMainWithDefaultVty (Just chan) app (initialState chan cfg)
  when (appState ^. #relaunch) $ Dyre.relaunchMaster Nothing

showError :: Config -> String -> Config
showError cfg s = cfg { dyreError = Just s }

annalise :: Config -> IO ()
annalise cfg = do
  libdir <- getLibDir
  let params = (Dyre.newParams "annalise" main showError)
               { Dyre.includeDirs = [libdir] }
  pathsConfig <- Dyre.getPathsConfig params
  Dyre.wrapMain params $ cfg {
    configFile = Dyre.configFile pathsConfig
  }
