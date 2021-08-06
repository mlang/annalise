{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
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
        , channelSize
        , theme
        , pgnDirectory
        , chessboardKeymap
        , configEditorKeymap
        , globalKeymap
        , onStartup
        , defaultView
        )
, sec
, ViewName(..)
, defaultConfig
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
import qualified Brick.Main                   as Brick
import           Brick.Types                  (BrickEvent (AppEvent, VtyEvent),
                                               EventM, Location (..), Next,
                                               Padding (..),
                                               ViewportType (Both, Horizontal, Vertical),
                                               Widget)
import           Brick.Widgets.Border         (border, borderWithLabel)
import           Brick.Widgets.Center         (hCenter, vCenter)
import           Brick.Widgets.Chess
import           Brick.Widgets.Core           (emptyWidget, fill, hBox, hLimit,
                                               hLimitPercent, padBottom,
                                               padLeft, padRight, padTop,
                                               showCursor, str, strWrap, txt,
                                               txtWrap, vBox, vLimit,
                                               vLimitPercent, viewport, (<+>),
                                               (<=>))
import           Brick.Widgets.Edit           (Editor, editContentsL, editor,
                                               getEditContents,
                                               handleEditorEvent, renderEditor)
import           Brick.Widgets.FileBrowser
import qualified Brick.Widgets.FileBrowser    as Brick
import qualified Brick.Widgets.List           as Brick
import           Brick.Widgets.Skylighting    (attrMappingsForStyle, highlight)
import qualified Config.Dyre                  as Dyre
import qualified Config.Dyre.Paths            as Dyre
import qualified Config.Dyre.Relaunch         as Dyre
import           Control.Applicative          ((<|>))
import           Control.Concurrent           (ThreadId, forkIO, killThread)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Lens                 (Getter, Getting, Lens',
                                               LensLike', Prism', _2, _Just,
                                               _Wrapped, assign, at, from, ix,
                                               lens, modifying, preuse, prism',
                                               reuse, to, traverseOf, use, uses,
                                               view, (%=), (%~), (&), (.=),
                                               (.~), (<>=), (<>~), (<~), (?~),
                                               (^.), (^?!), (^?))
import           Control.Lens.TH              (makeLenses)
import           Control.Monad                (forever, join, void, when)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.State          (StateT, evalStateT, execStateT,
                                               get, lift, modify, put)
import           Data.Bifunctor               (first, second)
import           Data.Binary                  (Binary)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import           Data.Char                    (toLower)
import           Data.FileEmbed               (embedFile)
import           Data.Foldable                (foldl', for_, toList)
import           Data.Functor                 ((<&>))
import           Data.List                    (elemIndex, find, intercalate,
                                               intersperse, isPrefixOf, sort)
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe,
                                               isJust)
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Zipper             as Zipper
import           Data.Tree                    (Tree (..), foldTree)
import           Data.Tree.NonEmpty           (forestFromList, pathTree)
import           Data.Tree.Zipper             (Full, TreePos, forest,
                                               fromForest, label, nextTree)
import qualified Data.Tree.Zipper             as TreePos
import qualified Data.Vector                  as Vector
import qualified Data.Vector.Unboxed          as Unboxed
import           GHC.Generics                 (Generic)
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
import           Time.Units                   (Microsecond, Second, Time, sec)

data ViewName = HelpView
              | ChessboardView
              | ExplorerView
              | ConfigEditorView
              deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Binary ViewName

data Name = Help
          | Chessboard
          | PlyList | FileBrowser | GameList
          | ConfigEditor | RebuildError
          deriving (Eq, Ord, Read, Show)

type AppEvent = [UCI.Info]

data Game = Game
  { _gInitial     :: Position
  , _gPlies       :: [Ply]
  , _gPerspective :: Maybe Color
  , _gInput       :: String
  , _gFrom        :: Maybe Square
  , _gCursor      :: Square
  } deriving (Generic)

instance Binary Game

gInitial :: Lens' Game Position
gInitial = lens _gInitial $ \s b -> s { _gInitial = b }

gPlies :: Lens' Game [Ply]
gPlies = lens _gPlies $ \s b -> s { _gPlies = b }

gPerspective :: Lens' Game (Maybe Color)
gPerspective = lens _gPerspective $ \s b -> s { _gPerspective = b }

gInput :: Lens' Game String
gInput = lens _gInput $ \s b -> s { _gInput = b }

gFrom :: Lens' Game (Maybe Square)
gFrom = lens _gFrom $ \s b -> s { _gFrom = b }

gCursor :: Lens' Game Square
gCursor = lens _gCursor $ \s b -> s { _gCursor = b }

newGame :: Game
newGame = Game startpos [] Nothing "" Nothing E1

togglePerspective :: Action ()
togglePerspective = withFocus go where
  go ChessboardView _ = do
    modifying (asGame . gPerspective) $ \case
      Nothing    -> Just White
      Just White -> Just Black
      Just Black -> Nothing

pgnGame :: Game -> PGN.Game
pgnGame g = PGN.gameFromForest [] ts PGN.Undecided where
  ts = forestFromList $ g ^. gPlies

data Analyser = Analyser
  { _aEngine :: UCI.Engine
  , _aReader :: Maybe (TChan UCI.BestMove, ThreadId)
  , _aPVs    :: Vector.Vector PredictedVariation
  }

aEngine :: Lens' Analyser UCI.Engine
aEngine = lens _aEngine $ \s b -> s { _aEngine = b }

aReader :: Lens' Analyser (Maybe (TChan UCI.BestMove, ThreadId))
aReader = lens _aReader $ \s b -> s { _aReader = b }

aPVs :: Lens' Analyser (Vector.Vector PredictedVariation)
aPVs = lens _aPVs $ \s b -> s { _aPVs = b}

data PredictedVariation = PV !UCI.Score !(Maybe UCI.Bounds) !(Unboxed.Vector Ply)

mkAnalyser :: MonadIO m => Config -> m (Maybe Analyser)
mkAnalyser Config{..} =
  liftIO (UCI.start' engineStartupTimeout (const $ pure ()) engineExecutable engineArgs) >>= \case
    Nothing -> pure Nothing
    Just e -> do
      UCI.setOptionSpinButton "MultiPV" 3 e
      pure . Just $
        Analyser e Nothing Vector.empty

data Explorer = Explorer
  { _eInitial     :: Position
  , _eTreePos     :: TreePos Full (NonEmpty Ply)
  , _eGameChooser :: Maybe GameChooser
  }

------------------------------------------------------------------------------

data GameChooser = GameChooser GameChooserStep

cgStep :: Lens' GameChooser GameChooserStep
cgStep = lens (\(GameChooser a) -> a) $ \(GameChooser _) b -> GameChooser b

data GameChooserStep = ChooseFile (Brick.FileBrowser Name)
                     | ChooseGame (Brick.FileBrowser Name, Brick.GenericList Name Seq PGN.Game)

_ChooseFile :: Prism' GameChooserStep (Brick.FileBrowser Name)
_ChooseFile = prism' ChooseFile $ \case
  ChooseFile a -> Just a
  _            -> Nothing

_ChooseGame :: Prism' GameChooserStep (Brick.FileBrowser Name, Brick.GenericList Name Seq PGN.Game)
_ChooseGame = prism' ChooseGame $ \case
  ChooseGame a -> Just a
  _            -> Nothing

newGameChooser :: Action GameChooser
newGameChooser = GameChooser . ChooseFile <$> newPGNBrowser

chooseGame :: PGN -> GameChooser -> GameChooser
chooseGame pgn (GameChooser (ChooseFile fb)) = GameChooser $ ChooseGame
  (fb, Brick.list GameList (Seq.fromList $ pgn ^. _Wrapped) 1)

chooseFile :: GameChooser -> GameChooser
chooseFile (GameChooser (ChooseGame (fb, _))) = GameChooser $ ChooseFile fb

------------------------------------------------------------------------------

eInitial :: Lens' Explorer Position
eInitial = lens _eInitial $ \s b -> s { _eInitial = b }

eTreePos :: Lens' Explorer (TreePos Full (NonEmpty Ply))
eTreePos = lens _eTreePos $ \s b -> s { _eTreePos = b }

eGameChooser :: Lens' Explorer (Maybe GameChooser)
eGameChooser = lens _eGameChooser $ \s b -> s { _eGameChooser = b }

defaultExplorer :: Explorer
defaultExplorer = Explorer { .. } where
  _eInitial = startpos
  _eTreePos = fromJust . nextTree . fromForest $
              pathTree <$> bookForest defaultBook _eInitial
  _eGameChooser = Nothing

data AppState = AppState
  { _asChannel      :: BChan AppEvent
  , _asConfig       :: Config
  , _asFocus        :: FocusRing ViewName
  , _asViewFocus    :: Map ViewName (FocusRing Name)
  , _asGame         :: Game
  , _asAnalyser     :: Maybe Analyser
  , _asExplorer     :: Explorer
  , _asConfigEditor :: Editor Text Name
  , _asMessage      :: Maybe Text
  , _asRelaunch     :: Bool
  }

type Action = StateT AppState (EventM Name)

data Binding = Binding
  { bindingDescription :: Text
  , bindingGuard       :: Action Bool
  , bindingAction      :: Action (Next AppState)
  }

simpleBinding :: Text -> Action (Next AppState) -> Binding
simpleBinding desc = Binding desc (pure True)

type Keymap = Map Vty.Event Binding

data Config = Config
  { engineExecutable     :: FilePath
  , engineArgs           :: [String]
  , engineStartupTimeout :: Time Second
  , channelSize          :: Int
  , theme                :: Brick.AttrMap
  , helpKeymap           :: Keymap
  , chessboardKeymap     :: Keymap
  , explorerKeymap       :: Keymap
  , pgnBrowserKeymap     :: Keymap
  , gameListKeymap       :: Keymap
  , pgnDirectory         :: Maybe FilePath
  , configEditorKeymap   :: Keymap
  , globalKeymap         :: Keymap
  , onStartup            :: Action ()
  , defaultView          :: ViewName
  , configFile           :: FilePath
  , dyreError            :: Maybe String
  }

themeL :: Lens' Config Brick.AttrMap
themeL = lens theme $ \s b -> s { theme = b }

configFileL :: Getter Config FilePath
configFileL = to configFile

helpKeymapL :: Lens' Config Keymap
helpKeymapL = lens helpKeymap $ \s b -> s { helpKeymap = b }

chessboardKeymapL :: Lens' Config Keymap
chessboardKeymapL = lens chessboardKeymap $ \s b -> s { chessboardKeymap = b }

explorerKeymapL :: Lens' Config Keymap
explorerKeymapL = lens explorerKeymap $ \s b -> s { explorerKeymap = b }

pgnBrowserKeymapL :: Getter Config Keymap
pgnBrowserKeymapL = to pgnBrowserKeymap

gameListKeymapL :: Lens' Config Keymap
gameListKeymapL = lens gameListKeymap $ \s b -> s { gameListKeymap = b }

pgnDirectoryL :: Lens' Config (Maybe FilePath)
pgnDirectoryL = lens pgnDirectory $ \s b -> s { pgnDirectory = b }

configEditorKeymapL :: Lens' Config Keymap
configEditorKeymapL = lens configEditorKeymap $ \s b ->
  s { configEditorKeymap = b }

globalKeymapL :: Lens' Config Keymap
globalKeymapL = lens globalKeymap $ \s b -> s { globalKeymap = b }

startupAction :: Getter Config (Action ())
startupAction = to onStartup

defaultViewL :: Lens' Config ViewName
defaultViewL = lens defaultView $ \s b -> s { defaultView = b }

defaultTheme :: Style -> Brick.AttrMap
defaultTheme sty = Brick.attrMap Vty.defAttr $
  [] <> attrMappingsForStyle sty

defaultConfig :: Config
defaultConfig = Config { .. } where
  engineExecutable = "stockfish"
  engineArgs = []
  engineStartupTimeout = sec 5
  channelSize = 20
  theme = defaultTheme breezeDark
  helpKeymap = defaultHelpKeymap
  chessboardKeymap = defaultChessboardKeymap
  explorerKeymap = defaultExplorerKeymap
  pgnBrowserKeymap = defaultPgnBrowserKeymap
  gameListKeymap = defaultGameListKeymap
  pgnDirectory = Nothing
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
          <> showDefault <> value (engineExecutable cfg)
          <> help "Path to the UCI engine executable") <*>
  pure (engineArgs cfg) <*>
  pure (engineStartupTimeout cfg) <*>
  option auto (long "channel-size"
            <> metavar "INT"
            <> help "Event channel size"
            <> showDefault <> value (channelSize cfg)) <*>
  pure (theme cfg) <*>
  pure (helpKeymap cfg) <*>
  pure (chessboardKeymap cfg) <*>
  pure (explorerKeymap cfg) <*>
  pure (pgnBrowserKeymap cfg) <*>
  pure (gameListKeymap cfg) <*>
  optional (strOption (long "pgn-directory" <> metavar "DIR" <> case pgnDirectory cfg of { Just dir -> showDefault <> value dir; Nothing -> mempty })) <*>
  pure (configEditorKeymap cfg) <*>
  pure (globalKeymap cfg) <*>
  pure (onStartup cfg) <*>
  pure (defaultView cfg) <*>
  pure (configFile cfg) <*>
  pure (dyreError cfg)

------------------------------------------------------------------------------

renderHelp s = [ui] where
  ui = header <=> vp where
    header = hCenter . border . padLeft (Pad 3) . padRight (Pad 3) $
             txt "annalise help"
    vp = viewport Help Vertical content
    content = vBox $ padTop (Pad 1) . r <$>
      [ s ^. asConfig . globalKeymapL
      , s ^. asConfig . chessboardKeymapL
      , s ^. asConfig . explorerKeymapL
      , s ^. asConfig . configEditorKeymapL
      ]
    r = Map.foldlWithKey f emptyWidget where
      f w k b = w <=> ((vLimit 1 . hLimit 20 $ txt (ppVtyEvent k) <+> fill ' ') <+> txtWrap (bindingDescription b))

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
helpHandler = EventHandler (asConfig . helpKeymapL) $ const continue

------------------------------------------------------------------------------

newPGNBrowser :: Action (FileBrowser Name)
newPGNBrowser = use (asConfig . pgnDirectoryL) >>= \dir ->
  liftIO (newFileBrowser selectNonDirectories FileBrowser dir) <&>
  setFileBrowserEntryFilter (Just $ fileExtensionMatch "pgn")

----------------------------------------------------------------------------------

ePlies :: Getter Explorer (NonEmpty Ply)
ePlies = eTreePos . to label

eCurrentPosition, ePreviousPosition :: Explorer -> Position
eCurrentPosition e = foldl' unsafeDoPly (e^.eInitial) (e^.ePlies)
ePreviousPosition e = foldl' unsafeDoPly (e^.eInitial) (e^.ePlies.to NonEmpty.init)

elemList :: Eq a => n -> a -> [a] -> Brick.List n a
elemList n x xs = Brick.list n (Vector.fromList xs) 1
                & Brick.listSelectedL .~ i where
  i = x `elemIndex` xs

ePlyList :: Explorer -> Brick.List Name Ply
ePlyList (view eTreePos -> tp) = elemList PlyList ply plies where
  ply = NonEmpty.last . label $ tp
  plies = fmap (NonEmpty.last . rootLabel) . forest $ tp

renderExplorer :: AppState -> [Widget Name]
renderExplorer s = go $ view asExplorer s where
  go e = fb <> [ui] where
    ui = (hLimit 9 list <+> hLimit 23 board <+> var)
     <=> fill ' '
     <=> renderMessage s
    list = withFocusRing' s ExplorerView
      (Brick.renderList (drawPly (ePreviousPosition e))) (ePlyList e)
    drawPly p foc = putCursorIf foc PlyList (0, 0) . str . toSAN p
    board = renderPosition Chessboard (eCurrentPosition e) (Just (color (ePreviousPosition e))) Nothing english True
    var = strWrap $ varToSAN (e ^. eInitial) (e ^. eTreePos . to label)
    fb = case e^.eGameChooser of
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

withFocusRing' s v f a = withFocusRing (s ^?! asViewFocus . ix v)
  f a

explorerHandler :: EventHandler
explorerHandler = EventHandler (asConfig . explorerKeymapL) $ \e -> do
  message $ UnboundEvent e
  continue

explorerPrev, explorerNext, explorerFirstChild, explorerParent :: Action ()
explorerPrev = asExplorer . eTreePos %= (fromMaybe <*> TreePos.prev)
explorerNext = asExplorer . eTreePos %= (fromMaybe <*> TreePos.next)
explorerFirstChild = asExplorer . eTreePos %= (fromMaybe <*> TreePos.firstChild)
explorerParent = asExplorer . eTreePos %= (fromMaybe <*> TreePos.parent)

createGame :: Action ()
createGame = do
  pos <- use $ asExplorer . eInitial
  plies <- use $ asExplorer . ePlies
  changeGame $ do
    asGame . gInitial .= pos
    asGame . gPlies .= toList plies
    sideToMove <- color <$> uses asGame currentPosition
    asGame . gCursor .= (if sideToMove == White then E1 else E8)
  switchView ChessboardView

setViewFocus :: ViewName -> Name -> Action ()
setViewFocus vn n = asViewFocus . ix vn %= focusSetCurrent n

eOpenFile :: Action ()
eOpenFile = do
  asExplorer . eGameChooser <~ Just <$> newGameChooser
  setViewFocus ExplorerView FileBrowser

abort :: Action ()
abort = withFocus go where
  go ExplorerView FileBrowser = eAbortOpenFile
  go ExplorerView GameList = do
    asExplorer . eGameChooser . _Just %= chooseFile
    setViewFocus ExplorerView FileBrowser
  go _ _                     = pure ()

eAbortOpenFile :: Action ()
eAbortOpenFile = do
  asExplorer . eGameChooser .= Nothing
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
    , simpleBinding "Create game from this variation" $ createGame *> continue
    )
  , ( Vty.EvKey (Vty.KChar 'o') [Vty.MCtrl]
    , simpleBinding "Open PGN file" $ eOpenFile *> continue
    )
  ]

------------------------------------------------------------------------------

pgnBrowserEnter :: Action ()
pgnBrowserEnter = use (asExplorer . eGameChooser) >>= \case
  Just (GameChooser (ChooseFile fb)) -> case Brick.fileBrowserCursor fb of
    Just fileInfo -> readPGNFile (fileInfoFilePath fileInfo) >>= \case
      Right pgn -> do
        asExplorer . eGameChooser . _Just %= chooseGame pgn
        setViewFocus ExplorerView GameList
      Left err -> message err
    Nothing -> pure ()
  Just (GameChooser (ChooseGame (_, l))) -> case snd <$> Brick.listSelectedElement l of
    Just game -> do
      let forest = pathTree . fmap (^. PGN.annPly) <$> game ^. PGN.cgForest
      case nextTree (fromForest forest) of
        Just treePos -> do
          asExplorer . eTreePos .= treePos
          eAbortOpenFile
        Nothing -> message ("Empty game" :: String)
    Nothing -> pure ()

doesPGNExist :: Action Bool
doesPGNExist = do
  fb <- preuse $ asExplorer . eGameChooser . _Just . cgStep . _ChooseFile
  maybe (pure False) (liftIO . doesFileExist . fileInfoFilePath) $
    fileBrowserCursor =<< fb

pgnBrowserIsSearching :: Action Bool
pgnBrowserIsSearching =
  preuse (asExplorer . eGameChooser . _Just . cgStep . _ChooseFile) <&>
  maybe False fileBrowserIsSearching

pgnBrowserHandler :: EventHandler
pgnBrowserHandler = EventHandler (asConfig . pgnBrowserKeymapL) $ \e -> do
  handleEvent (asExplorer . eGameChooser . _Just . cgStep . _ChooseFile) handleFileBrowserEvent e
  continue


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
gameListHandler = EventHandler (asConfig . gameListKeymapL) $ \e -> do
  handleEvent (asExplorer . eGameChooser . _Just . cgStep . _ChooseGame . _2)
    Brick.handleListEvent e
  continue

------------------------------------------------------------------------------

toggleAnalyser :: Action ()
toggleAnalyser = do
  use asAnalyser >>= \case
    Nothing -> startAnalyser
    Just a -> do
      stopSearch
      liftIO $ UCI.quit (a^.aEngine)
      asAnalyser .= Nothing

startAnalyser :: Action ()
startAnalyser = do
  use asConfig >>= mkAnalyser >>= \case
    Nothing -> message ("Failed to start engine" :: String)
    Just a -> do
      asAnalyser .= Just a
      startSearch

startSearch :: Action ()
startSearch = do
  use asAnalyser >>= \case
    Just a -> do
      pos <- use $ asGame.gInitial
      UCI.setPosition (a^.aEngine) pos =<< use (asGame.gPlies)
      (bmc, ic) <- UCI.search (a^.aEngine) [UCI.infinite]
      chan <- use asChannel
      tid <- liftIO . forkIO . forever $
        atomically (readTChan ic) >>= writeBChanNonBlocking chan
      asAnalyser .= Just (a & aReader ?~ (bmc, tid))
    Nothing -> pure ()

stopSearch :: Action Bool
stopSearch = do
  use asAnalyser >>= \case
    Just a -> case a^.aReader of
      Just (bmc, tid) -> do
        liftIO $ do
          killThread tid
          let e = a^.aEngine
          UCI.stop e
          void . atomically $ readTChan bmc
          UCI.isready e
        asAnalyser . traverse . aReader .= Nothing
        asAnalyser . traverse . aPVs     .= Vector.empty
        pure True
      Nothing -> pure False
    Nothing -> pure False

changeGame :: Action a -> Action a
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
  edit = [renderEditor (highlight haskellSyntax . Text.unlines) True (_asConfigEditor s)]
  err = case s ^. asConfig . to dyreError of
    Nothing -> []
    Just e -> [borderWithLabel (str "Rebuild error") $
               viewport RebuildError Both $
               str e]
  msg = [renderMessage s]
  stat = [vLimit 1 $ str "C-q to quit"]

renderMessage :: AppState -> Widget n
renderMessage s = maybe (str "") txtWrap $ s ^. asMessage

configEditorText :: Getter AppState Text
configEditorText = asConfigEditor . to getEditContents . to Text.unlines

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
clearMessage = asMessage .= Nothing

class Message a where
  message :: a -> Action ()

instance Message Text where
  message = assign asMessage . Just

instance Message String where
  message = message . Text.pack

data Info = UnboundEvent Vty.Event
          | WroteFile FilePath
          | SeveralPiecesCanMoveHere Square Position [Ply]
          | NotAllowedToMove Color PieceType Square
          deriving (Eq, Generic, Show)

instance Message Info where
  message (UnboundEvent e) = message $ "Unbound event: " <> ppVtyEvent e
  message (WroteFile fp)   = message $ "Wrote " <> fp
  message (SeveralPiecesCanMoveHere sq pos pls) = message $
    "Several pieces can move to " <> show sq <> ": " <> intercalate "|" (fmap (toSAN pos) pls)
  message (NotAllowedToMove c p sq) = message $
    "You are not allowed to move the " <> show c <> " " <> show p <> " at " <> show sq

writeConfigFile :: Action ()
writeConfigFile = do
  fp <- use $ asConfig . configFileL
  liftIO . ByteString.writeFile fp . Text.encodeUtf8 =<< use configEditorText
  message $ WroteFile fp

reloadConfigFile :: Bool -> Action ()
reloadConfigFile verbose = do
  fp <- use $ asConfig . configFileL
  exists <- liftIO $ doesFileExist fp
  a <- Text.decodeUtf8 <$>
       if | exists    -> liftIO $ ByteString.readFile fp
          | otherwise -> pure $(embedFile "app/Main.hs")
  asConfigEditor .= editor ConfigEditor Nothing a
  when (verbose && exists) $
    message $ "Config loaded from " <> fp

gotoBeginningOfConfig, gotoEndOfConfig :: Action ()
gotoBeginningOfConfig = modifying (asConfigEditor . editContentsL) Zipper.gotoBOF
gotoEndOfConfig       = modifying (asConfigEditor . editContentsL) Zipper.gotoEOF

vScrollBy :: Name -> Int -> Action ()
vScrollBy n = lift . Brick.vScrollBy (Brick.viewportScroll n)

scrollRebuildErrorUp, scrollRebuildErrorDown :: Action ()
scrollRebuildErrorUp = vScrollBy RebuildError (-1)
scrollRebuildErrorDown = vScrollBy RebuildError 1

continue :: Action (Next AppState)
continue = lift . Brick.continue =<< get

relaunch :: Action (Next AppState)
relaunch = do
  persist
  asRelaunch .= True
  quit

quit :: Action (Next AppState)
quit = lift . Brick.halt =<< get

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
  (Vty.Event -> Action (Next AppState))

handleEvent :: LensLike' (EventM Name) AppState a
            -> (Vty.Event -> a -> EventM Name a)
            -> Vty.Event
            -> Action ()
handleEvent l h e = put =<< lift . traverseOf l (h e) =<< get

configEditorHandler :: EventHandler
configEditorHandler = EventHandler (asConfig . configEditorKeymapL) $ \e -> do
  handleEvent asConfigEditor handleEditorEvent e
  continue

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
chessboardHandler = EventHandler (asConfig . chessboardKeymapL) $ \e -> do
  message $ UnboundEvent e
  continue

resetGame :: Action ()
resetGame = changeGame $ asGame .= newGame

clearPlyInput :: Action ()
clearPlyInput = asGame . gInput .= ""

plyInput :: Char -> Action ()
plyInput c = do
  ok <- validPlyInput c <$> use asGame
  if ok then do
    asGame . gInput <>= pure c
    uses asGame selectedPly >>= \case
      Just ply -> do
        clearPlyInput
        addPly ply
      Nothing -> pure ()
  else use (asGame . gInput) >>= \case
    "" -> message $ "No ply starts with '" <> Text.singleton c <> "'"
    _  -> message $ "No ply continues with '" <> Text.singleton c <> "'"

validPlyInput :: Char -> Game -> Bool
validPlyInput c = not . null . selectedPlies . (gInput <>~ [c])

plyInputBS :: Action ()
plyInputBS = modifying (asGame . gInput) $ \case
  "" -> ""
  s  -> init s

selectedPly :: Game -> Maybe Ply
selectedPly g = if ok then Just $ head plies else Nothing where
  ok = not (null $ g ^. gInput) && length plies == 1
  plies = selectedPlies g

selectedPlies :: Game -> [Ply]
selectedPlies g =
  filter ((input `isPrefixOf`) . map toLower . toSAN pos) $ legalPlies pos
 where
  pos = currentPosition g
  input = g ^. gInput

takeback :: Action ()
takeback = changeGame . modifying (asGame . gPlies) $ \case
  [] -> []
  xs -> init xs

handleViewEvent :: ViewName -> Name
                -> AppState -> Vty.Event -> EventM Name (Next AppState)
handleViewEvent = go where
  go HelpView         _       = dispatch helpHandler
  go ChessboardView   _       = dispatch chessboardHandler
  go ExplorerView     PlyList = dispatch explorerHandler
  go ExplorerView FileBrowser = dispatch pgnBrowserHandler
  go ExplorerView GameList    = dispatch gameListHandler
  go ConfigEditorView _       = dispatch configEditorHandler
  dispatch (EventHandler keymapL fallback) s e = evalStateT action s where
    local = case Map.lookup e (s ^. keymapL) of
      Just b -> do
        g <- bindingGuard b
        if g then bindingAction b else fallback e
      Nothing -> fallback e
    action = do
      clearMessage
      case Map.lookup e (s ^. asConfig . globalKeymapL) of
        Just b -> do
          g <- bindingGuard b
          if g then bindingAction b else local
        Nothing -> local

------------------------------------------------------------------------------

currentPosition, previousPosition :: Game -> Position
currentPosition g = foldl' unsafeDoPly (g ^. gInitial) (g ^. gPlies)
previousPosition g = foldl' unsafeDoPly (g ^. gInitial) (init (g ^. gPlies))

cursorMod :: (Bool -> (Rank, File) -> (Rank, File)) -> Action ()
cursorMod f = do
  normal <- (== White) <$> uses asGame (color . currentPosition)
  modifying (asGame . gCursor) $ \sq ->
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
  asGame . gInput .= ""
  asGame . gFrom .= Nothing

enter :: Action ()
enter = do
  ok <- uses (asGame . gInput) null
  when ok $ do
    use (asGame . gFrom) >>= \case
      Nothing -> do
        sq <- use $ asGame . gCursor
        let goesTo pl = plyTarget pl == sq
        pos <- uses asGame currentPosition
        let plies = legalPlies pos
        case filter goesTo plies of
          [] -> do
            let comesFrom pl = plySource pl == sq
            case filter comesFrom plies of
              [] -> case pieceAt pos sq of
                Nothing     -> message $ "No piece can go to " <> show sq
                Just (c, p) -> message $ NotAllowedToMove c p sq
              [pl] -> addPly pl
              xs -> assign (asGame . gFrom) $ Just sq
          [pl] -> addPly pl
          xs -> message $ SeveralPiecesCanMoveHere sq pos xs
      Just src -> use (asGame . gCursor) >>= \dst -> do
        let p pl = plySource pl == src && plyTarget pl == dst
        plies <- legalPlies <$> uses asGame currentPosition
        case filter p plies of
          [] -> do
            message $ show dst <> " is not a valid target square"
          [pl] -> addPly pl
          _ -> pure ()

addPly :: Ply -> Action ()
addPly pl = do
  stopSearch
  asGame . gPlies <>= [pl]
  asGame . gFrom .= Nothing
  asGame . gInput .= ""
  startSearch

renderGame :: AppState -> [Widget Name]
renderGame s = [w $ s ^. asGame] where
  status = renderMessage s
  pv = case s ^? asAnalyser . traverse . aPVs of
    Just pvs -> let pos = s ^. asGame . to currentPosition
                in vBox $ drawPV pos <$> toList pvs
    _              -> emptyWidget
  drawPV pos (PV s b pv) = hLimit 10 (padLeft Max (drawScore s)) <+> str " " <+> str (varToSAN pos pv)
  drawScore = \case
    UCI.CentiPawns s -> str . show $ realToFrac s / 100
    UCI.MateIn hm | hm >= 0 -> str $ "#" <> show hm
                  | otherwise -> str $ "#" <> show hm

  w g = (hLimit 23 (hCenter board) <+> var)
    <=> (hLimit 21 . vLimit 1 $ sideToMove <+> fill ' ' <+> lastPly)
    <=> input
    <=> pv
    <=> fill ' '
    <=> status
   where
    board = renderPosition Chessboard pos (g ^. gPerspective) cursor english (null $ g ^. gInput)
    cursor | null (g ^. gInput) = Just $ g ^. gCursor
           | otherwise          = Nothing
    var = strWrap . varToSAN (g ^. gInitial) $ g ^. gPlies
    sideToMove = str . show . color $ pos
    lastPly = case g ^. gPlies of
      [] -> str "START"
      ps -> str (show (moveNumber pos))
        <+> str (if color pos == Black then "." else "...")
        <+> str (toSAN (previousPosition g) (last ps))
    input = case g ^. gInput of
      "" -> str " "
      input -> showCursor Chessboard (Location (length input, 0)) (str input)
           <+> str "{" <+> plies <+> str "}"
    plies = hBox . intersperse (str " ") $
      str <$> sort (toSAN pos <$> selectedPlies g)
    pos = currentPosition g

------------------------------------------------------------------------------

asChannel :: Getter AppState (BChan AppEvent)
asChannel = to _asChannel

asFocusL :: Lens' AppState (FocusRing ViewName)
asFocusL = lens _asFocus $ \s b -> s { _asFocus = b }

asConfig :: Lens' AppState Config
asConfig = lens _asConfig $ \s b -> s { _asConfig = b }

asViewFocus :: Lens' AppState (Map ViewName (FocusRing Name))
asViewFocus = lens _asViewFocus $ \s b -> s { _asViewFocus = b }

asGame :: Lens' AppState Game
asGame = lens _asGame $ \s b -> s { _asGame = b }

asAnalyser :: Lens' AppState (Maybe Analyser)
asAnalyser = lens _asAnalyser $ \s b -> s { _asAnalyser = b }

asExplorer :: Lens' AppState Explorer
asExplorer = lens _asExplorer $ \s b -> s { _asExplorer = b }

asConfigEditor :: Lens' AppState (Editor Text Name)
asConfigEditor = lens _asConfigEditor $ \s b -> s { _asConfigEditor = b }

asMessage :: Lens' AppState (Maybe Text)
asMessage = lens _asMessage $ \s b -> s { _asMessage = b }

asRelaunch :: Lens' AppState Bool
asRelaunch = lens _asRelaunch $ \s b -> s { _asRelaunch = b }

initialState :: BChan AppEvent -> Config -> AppState
initialState chan cfg =
  AppState chan cfg focus viewFocus newGame Nothing defaultExplorer edit Nothing False
 where
  focus = focusSetCurrent (defaultView cfg) (focusRing [minBound .. maxBound])
  viewFocus = Map.fromList
    [ ( ChessboardView, focusRing [Chessboard] )
    , ( ExplorerView, focusRing [PlyList, FileBrowser, GameList] )
    , ( ConfigEditorView, focusRing [ConfigEditor] )
    ]
  edit = editor ConfigEditor Nothing (Text.decodeUtf8 $(embedFile "app/Main.hs"))

focusedView :: Getter AppState (Maybe ViewName)
focusedView = asFocusL . to focusGetCurrent

setView :: ViewName -> AppState -> AppState
setView v = asFocusL %~ focusSetCurrent v

viewName :: AppState -> ViewName
viewName s = fromMaybe (s ^. asConfig . defaultViewL) $ s ^. focusedView

withFocus :: (ViewName -> Name -> Action a) -> Action a
withFocus f = do
  vn <- fromJust . focusGetCurrent <$> use asFocusL
  n <- fromJust . focusGetCurrent . fromJust <$> use (asViewFocus . at vn)
  f vn n

focusedWidget :: AppState -> Maybe (FocusRing Name)
focusedWidget s = s ^. asViewFocus . at (viewName s)

addPV i score bounds pv pvs
  | i == Vector.length pvs
  = pvs <> Vector.singleton (PV score bounds pv)
  | otherwise
  = pvs Vector.// [(i, PV score bounds pv)]

app :: Brick.App AppState AppEvent Name
app = Brick.App { .. } where
  appStartEvent = execStateT $ do
    reloadConfigFile False
    restore
    join . use $ asConfig . startupAction
  appDraw s = renderView (viewName s) s
  appHandleEvent s = go where
    go (VtyEvent e) = handleViewEvent vn n s e where
      vn = viewName s
      n = fromJust . focusGetCurrent $ s ^?! asViewFocus . ix vn
    go (AppEvent i) = case (find isMultiPV i, find isScore i, find isPV i) of
      (Just (UCI.MultiPV i'), Just (UCI.Score score bounds), Just (UCI.PV pv)) ->
        Brick.continue $
        s & asAnalyser . traverse . aPVs %~ addPV (pred i') score bounds pv
      _ -> Brick.continue s
    go _            = Brick.continue s
  appAttrMap = view $ asConfig . themeL
  appChooseCursor s
    | isJust $ s ^? asViewFocus . ix (viewName s)
    = focusRingCursor (^?! asViewFocus . ix (viewName s)) s
    | otherwise
    = Brick.neverShowCursor s

isScore UCI.Score{} = True
isScore _           = False
isPV UCI.PV{} = True
isPV _        = False
isMultiPV UCI.MultiPV{} = True
isMultiPV _             = False

------------------------------------------------------------------------------

data Persistent = Persistent ViewName Game
                  deriving (Generic)

instance Binary Persistent

persistent :: Action Persistent
persistent =
  Persistent <$> use (asFocusL . to focusGetCurrent . to fromJust)
             <*> use asGame

persist, restore :: Action ()
persist = liftIO . Dyre.saveBinaryState =<< persistent
restore = do
  (Persistent vn g) <- liftIO . Dyre.restoreBinaryState =<< persistent
  asFocusL %= focusSetCurrent vn
  asGame .= g

------------------------------------------------------------------------------

main :: Config -> IO ()
main inCfg = do
  cfg <- execParser $ info (commandLine inCfg <**> helper) $ briefDesc
  chan <- newBChan (channelSize cfg)
  let buildVty = Vty.mkVty Vty.defaultConfig
  vty <- buildVty
  appState <- Brick.customMain vty buildVty (Just chan) app (initialState chan cfg)
  when (appState ^. asRelaunch) $ Dyre.relaunchMaster Nothing

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
