{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Annalise (
  Config( engineExecutable
        , engineArgs
        , channelSize
        , theme
        , chessboardKeymap
        , configEditorKeymap
        , globalKeymap
        , onStartup
        )
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

import qualified Brick.AttrMap             as Brick
import           Brick.BChan               (BChan, newBChan, writeBChanNonBlocking)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import           Brick.Focus               (FocusRing, focusGetCurrent,
                                            focusRing, focusRingCursor,
                                            focusSetCurrent)
import Data.Foldable (for_)
import           Time.Rational (KnownDivRat)
import           Time.Units (Microsecond, sec, Time)
import qualified Brick.Main                as Brick
import Data.Vector.Unboxed (Vector)
import Control.Concurrent (forkIO, killThread, ThreadId)
import           Brick.Types               (BrickEvent (AppEvent, VtyEvent),
                                            EventM, Location (..), Next,
                                            ViewportType (Both, Horizontal, Vertical),
                                            Widget, handleEventLensed)
import           Brick.Widgets.Border      (borderWithLabel)
import           Brick.Widgets.Chess
import           Brick.Widgets.Core        (fill, hBox, hLimit, showCursor, str,
                                            txt, txtWrap, vBox, vLimit,
                                            viewport, (<+>), (<=>))
import           Brick.Widgets.Edit        (Editor, editContentsL, editor,
                                            getEditContents, handleEditorEvent,
                                            renderEditor)
import           Brick.Widgets.Skylighting (attrMappingsForStyle, highlight)
import qualified Config.Dyre               as Dyre
import qualified Config.Dyre.Paths         as Dyre
import qualified Config.Dyre.Relaunch      as Dyre
import           Control.Applicative       ((<|>))
import           Control.Lens              ((^?), (?~), Getter, Getting, Lens', assign, at,
                                            from, lens, modifying, to, use,
                                            uses, view, (%~), (&), (.=), (.~),
                                            (<>=), (^.))
import           Control.Monad             (forever, join, void, when)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.State       (StateT, evalStateT, execStateT, get,
                                            lift, modify, put)
import           Data.Bifunctor            (first, second)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as ByteString
import           Data.Char                 (toLower)
import           Data.FileEmbed            (embedFile)
import           Data.Functor              ((<&>))
import           Data.List                 (find, intersperse, isPrefixOf, sort)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, fromMaybe, isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.Zipper          as Zipper
import           Game.Chess
import  qualified       Game.Chess.UCI as UCI
import           Game.Chess.SAN
import qualified Graphics.Vty              as Vty
import           Options.Applicative       hiding (str)
import           Paths_annalise            (getLibDir)
import           Skylighting
import           System.Directory          (doesFileExist)

data Config = Config
  { engineExecutable   :: FilePath
  , engineArgs         :: [String]
  , channelSize        :: Int
  , theme              :: Brick.AttrMap
  , chessboardKeymap   :: Keymap
  , configEditorKeymap :: Keymap
  , globalKeymap       :: Keymap
  , onStartup          :: Action ()
  , configFile         :: FilePath
  , dyreError          :: Maybe String
  }

class HasTheme a where
  themeL :: Lens' a Brick.AttrMap

instance HasTheme Config where
  themeL = lens theme $ \s b -> s { theme = b }

class HasConfigFile a where
  configFileL :: Getter a FilePath

instance HasConfigFile Config where
  configFileL = to configFile

class HasChessboardKeymap a where
  chessboardKeymapL :: Lens' a Keymap

instance HasChessboardKeymap Config where
  chessboardKeymapL = lens chessboardKeymap $ \s b -> s { chessboardKeymap = b }

class HasConfigEditorKeymap a where
  configEditorKeymapL :: Lens' a Keymap

instance HasConfigEditorKeymap Config where
  configEditorKeymapL = lens configEditorKeymap $ \s b ->
    s { configEditorKeymap = b }

class HasGlobalKeymap a where
  globalKeymapL :: Lens' a Keymap

instance HasGlobalKeymap Config where
  globalKeymapL = lens globalKeymap $ \s b -> s { globalKeymap = b }

class HasStartupAction a where
  startupAction :: Getter a (Action ())

instance HasStartupAction Config where
  startupAction = to onStartup

defaultTheme :: Style -> Brick.AttrMap
defaultTheme sty = Brick.attrMap Vty.defAttr $
  [] <> attrMappingsForStyle sty

defaultConfig :: Config
defaultConfig = Config { .. } where
  engineExecutable = "stockfish"
  engineArgs = []
  channelSize = 20
  theme = defaultTheme breezeDark
  chessboardKeymap = defaultChessboardKeymap
  configEditorKeymap = defaultConfigEditorKeymap
  globalKeymap = defaultGlobalKeymap
  onStartup = pure ()
  configFile = undefined
  dyreError = Nothing

commandLine :: Config -> Parser Config
commandLine cfg = Config <$>
  strOption (long "engine-executable"
          <> metavar "NAME"
          <> showDefault <> value (engineExecutable cfg)
          <> help "Path to the UCI engine executable") <*>
  pure (engineArgs cfg) <*>
  option auto (long "channel-size"
            <> metavar "INT"
            <> help "Event channel size"
            <> showDefault <> value (channelSize cfg)) <*>
  pure (theme cfg) <*>
  pure (chessboardKeymap cfg) <*>
  pure (configEditorKeymap cfg) <*>
  pure (globalKeymap cfg) <*>
  pure (onStartup cfg) <*>
  pure (configFile cfg) <*>
  pure (dyreError cfg)

----------------------------------------------------------------------------------

data Analyser = Analyser
  { _aEngine :: UCI.Engine
  , _aReader :: Maybe (TChan UCI.BestMove, ThreadId)
  , _aPV :: Maybe (Vector Ply)
  }

aEngine :: Lens' Analyser UCI.Engine
aEngine = lens _aEngine $ \s b -> s { _aEngine = b }

aReader :: Lens' Analyser (Maybe (TChan UCI.BestMove, ThreadId))
aReader = lens _aReader $ \s b -> s { _aReader = b }

aPV :: Lens' Analyser (Maybe (Vector Ply))
aPV = lens _aPV $ \s b -> s { _aPV = b }

mkAnalyser :: MonadIO m => Config -> m (Maybe Analyser)
mkAnalyser Config{..} =
  fmap mk <$> liftIO (UCI.start' tout silence engineExecutable engineArgs)
 where
  mk e = Analyser e Nothing Nothing
  silence = const $ pure ()
  tout = sec 5

toggleAnalyser :: Action ()
toggleAnalyser = do
  use asAnalyser >>= \case
    Nothing -> startAnalyser
    Just a -> do
      stopSearch
      liftIO $ UCI.quit (a ^. aEngine)
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
      chan <- use asChannel
      liftIO $ UCI.setPosition (a ^. aEngine) startpos
      plies <- use $ asGame . gPlies
      for_ plies $ \pl -> UCI.addPly (a ^. aEngine) pl
      (bmc, ic) <- UCI.search (a ^. aEngine) [UCI.infinite]
      tid <- liftIO . forkIO . forever $
        atomically (readTChan ic) >>= writeBChanNonBlocking chan
      asAnalyser .= Just (a & aReader .~ Just (bmc, tid))
    Nothing -> pure ()

stopSearch :: Action Bool
stopSearch = do
  use asAnalyser >>= \case
    Just a -> case a ^. aReader of
      Just (bmc, tid) -> do
        liftIO $ do
          killThread tid
          let e = a ^. aEngine
          UCI.stop e
          void . atomically $ readTChan bmc
          UCI.isready e
        asAnalyser . traverse . aReader .= Nothing
        asAnalyser . traverse . aPV     .= Nothing
        pure True
      Nothing -> pure False
    Nothing -> pure False

data ViewName = ChessboardView | ConfigEditorView deriving (Eq, Ord, Read, Show)
type Action = StateT AppState (EventM Name)
type Keymap = Map Vty.Event (Action (Next AppState))

haskellSyntax :: Syntax
haskellSyntax = fromJust $ "haskell" `lookupSyntax` defaultSyntaxMap

renderView :: ViewName -> AppState -> [Widget Name]
renderView ConfigEditorView s = [vBox $ edit <> err <> msg <> stat] where
  edit = [renderEditor (highlight haskellSyntax . Text.unlines) True (_asConfigEditor s)]
  err = case dyreError $ _asConfig s of
    Nothing -> []
    Just e -> [borderWithLabel (str "Rebuild error") $
               viewport RebuildError Both $
               str e]
  msg = case s ^. asMessage of
    Nothing -> []
    Just t  -> [txtWrap t]
  stat = [vLimit 1 $ str "C-q to quit"]
renderView ChessboardView s = renderGame s

configEditorText :: Getter AppState Text
configEditorText = asConfigEditor . to getEditContents . to Text.unlines

defaultGlobalKeymap :: Keymap
defaultGlobalKeymap = Map.fromList
  [ ( Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]
    , quit
    )
  , ( Vty.EvKey (Vty.KFun 2) []
    , switchView ChessboardView *> continue
    )
  , ( Vty.EvKey (Vty.KFun 10) []
    , switchView ConfigEditorView *> continue
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

writeConfigFile :: Action ()
writeConfigFile = do
  fp <- use configFileL
  liftIO . ByteString.writeFile fp . Text.encodeUtf8 =<< use configEditorText
  message $ "Wrote " <> fp

reloadConfigFile :: Bool -> Action ()
reloadConfigFile verbose = do
  fp <- use configFileL
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

continue :: Action (Next AppState)
continue = get >>= lift . Brick.continue

relaunch :: Action (Next AppState)
relaunch = asRelaunch .= True >> quit

quit :: Action (Next AppState)
quit = get >>= lift . Brick.halt

defaultConfigEditorKeymap :: Keymap
defaultConfigEditorKeymap = Map.fromList
  [ ( Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]
    , writeConfigFile *> relaunch
    )
  , ( Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]
    , writeConfigFile *> continue
    )
  , ( Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]
    , reloadConfigFile True *> continue
    )
  , ( Vty.EvKey (Vty.KChar '<') [Vty.MMeta]
    , gotoBeginningOfConfig *> continue
    )
  , ( Vty.EvKey (Vty.KChar '>') [Vty.MMeta]
    , gotoEndOfConfig *> continue
    )
  ]

data EventHandler = EventHandler
  (Getter AppState Keymap)
  (Vty.Event -> Action (Next AppState))

handleEvent :: Lens' AppState a
            -> (Vty.Event -> a -> EventM Name a)
            -> Vty.Event
            -> Action ()
handleEvent l h e = get >>= \s -> lift (handleEventLensed s l h e) >>= put

configEditorHandler :: EventHandler
configEditorHandler = EventHandler configEditorKeymapL $ \e -> do
  handleEvent asConfigEditor handleEditorEvent e
  continue

defaultChessboardKeymap :: Keymap
defaultChessboardKeymap = Map.fromList $
  ([ ( Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl], toggleAnalyser *> continue )
   , ( Vty.EvKey Vty.KLeft [], cursorLeft *> continue )
   , ( Vty.EvKey Vty.KRight [], cursorRight *> continue )
   , ( Vty.EvKey Vty.KUp [], cursorUp *> continue )
   , ( Vty.EvKey Vty.KDown [], cursorDown *> continue )
   , ( Vty.EvKey Vty.KEnter [], enter *> continue )
   , ( Vty.EvKey Vty.KBS [], plyInputBS *> continue )
   , ( Vty.EvKey Vty.KEsc [], clearInput *> continue )
   , ( Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl], resetGame *> continue )
   ] <>
  ) $
  "abcdefgh12345678knqrxo" <&> \c ->
  ( Vty.EvKey (Vty.KChar c) []
  , plyInput c *> continue
  )

chessboardHandler :: EventHandler
chessboardHandler = EventHandler chessboardKeymapL $ \e -> do
  message $ "Unbound: " <> show e
  continue

resetGame :: Action ()
resetGame = do
  stopped <- stopSearch
  asGame .= newGame
  when stopped $ startSearch

clearPlyInput :: Action ()
clearPlyInput = asGame . gInput .= ""

plyInput :: Char -> Action ()
plyInput c = do
  asGame . gInput <>= pure c
  uses asGame selectedPly >>= \case
    Just ply -> do
      clearPlyInput
      addPly ply
    Nothing -> pure ()

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
  filter ((input `isPrefixOf`) . fmap toLower . toSAN pos) $ legalPlies pos
 where
  pos = currentPosition g
  input = g ^. gInput

dispatch :: EventHandler -> AppState -> Vty.Event -> EventM Name (Next AppState)
dispatch (EventHandler keymapL fallback) s e = evalStateT action s where
  action = clearMessage *> case Map.lookup e (s ^. globalKeymapL)
                            <|> Map.lookup e (s ^. keymapL) of
    Just a  -> a
    Nothing -> fallback e

handleViewEvent :: ViewName -> AppState -> Vty.Event -> EventM Name (Next AppState)
handleViewEvent ChessboardView   = dispatch chessboardHandler
handleViewEvent ConfigEditorView = dispatch configEditorHandler

data Game = Game
  { _gInitial :: Position
  , _gPlies   :: [Ply]
  , _gInput   :: String
  , _gFrom    :: Maybe Square
  , _gCursor  :: Maybe Square
  }

gInitial :: Lens' Game Position
gInitial = lens _gInitial $ \s b -> s { _gInitial = b }

gPlies :: Lens' Game [Ply]
gPlies = lens _gPlies $ \s b -> s { _gPlies = b }

gInput :: Lens' Game String
gInput = lens _gInput $ \s b -> s { _gInput = b }

gFrom :: Lens' Game (Maybe Square)
gFrom = lens _gFrom $ \s b -> s { _gFrom = b }

gCursor :: Lens' Game (Maybe Square)
gCursor = lens _gCursor $ \s b -> s { _gCursor = b }

newGame :: Game
newGame = Game startpos [] "" Nothing (Just E1)

currentPosition, previousPosition :: Game -> Position
currentPosition g = foldl doPly (g ^. gInitial) (g ^. gPlies)
previousPosition g = foldl doPly (g ^. gInitial) (init (g ^. gPlies))

cursorMod :: (Bool -> (Rank, File) -> (Rank, File)) -> Action ()
cursorMod f = do
  normal <- (== White) <$> uses asGame (color . currentPosition)
  modifying (asGame . gCursor) . fmap $ \sq ->
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
      Nothing -> use (asGame . gCursor) >>= assign (asGame . gFrom)
      Just src -> use (asGame . gCursor) >>= \case
        Nothing -> pure ()
        Just dst -> do
          let p pl = plySource pl == src && plyTarget pl == dst
          plies <- legalPlies <$> uses asGame currentPosition
          let cand = filter p plies
          case cand of
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
  status = case s ^. asMessage of
    Just msg -> txtWrap msg
    Nothing -> str ""
  pv = case s ^? asAnalyser . traverse . aPV of
    Just (Just pv) -> str $ varToSAN (s ^. asGame . to currentPosition) pv
    _ -> str ""
  w g = board
    <=> (hLimit 21 . vLimit 1 $ sideToMove <+> fill ' ' <+> lastPly)
    <=> input
    <=> pv
    <=> fill ' '
    <=> status
   where
    board = renderPosition Chessboard pos Nothing cursor english (null $ g ^. gInput)
    cursor | null (g ^. gInput) = g ^. gCursor
           | otherwise          = Nothing
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

data AppState = AppState
  { _asChannel      :: BChan AppEvent
  , _asConfig       :: Config
  , _asFocus        :: FocusRing ViewName
  , _asViewFocus    :: Map ViewName (FocusRing Name)
  , _asGame         :: Game
  , _asAnalyser :: Maybe Analyser
  , _asConfigEditor :: Editor Text Name
  , _asMessage      :: Maybe Text
  , _asRelaunch     :: Bool
  }

asChannel :: Lens' AppState (BChan AppEvent)
asChannel = lens _asChannel $ \s b -> s { _asChannel = b }

asFocusL :: Lens' AppState (FocusRing ViewName)
asFocusL = lens _asFocus $ \s b -> s { _asFocus = b }

asConfig :: Lens' AppState Config
asConfig = lens _asConfig $ \s b -> s { _asConfig = b }

asViewFocusL :: Lens' AppState (Map ViewName (FocusRing Name))
asViewFocusL = lens _asViewFocus $ \s b -> s { _asViewFocus = b }

asGame :: Lens' AppState Game
asGame = lens _asGame $ \s b -> s { _asGame = b }

asAnalyser :: Lens' AppState (Maybe Analyser)
asAnalyser = lens _asAnalyser $ \s b -> s { _asAnalyser = b }

asConfigEditor :: Lens' AppState (Editor Text Name)
asConfigEditor = lens _asConfigEditor $ \s b -> s { _asConfigEditor = b }

instance HasTheme AppState where
  themeL = asConfig . themeL

instance HasConfigFile AppState where
  configFileL = asConfig . configFileL

instance HasChessboardKeymap AppState where
  chessboardKeymapL = asConfig . chessboardKeymapL

instance HasConfigEditorKeymap AppState where
  configEditorKeymapL = asConfig . configEditorKeymapL

instance HasGlobalKeymap AppState where
  globalKeymapL = asConfig . globalKeymapL

instance HasStartupAction AppState where
  startupAction = asConfig . startupAction

asMessage :: Lens' AppState (Maybe Text)
asMessage = lens _asMessage $ \s b -> s { _asMessage = b }

asRelaunch :: Lens' AppState Bool
asRelaunch = lens _asRelaunch $ \s b -> s { _asRelaunch = b }

initialState :: BChan AppEvent -> Config -> AppState
initialState chan cfg =
  AppState chan cfg focus viewFocus newGame Nothing edit Nothing False
 where
  focus = focusRing [ChessboardView, ConfigEditorView]
  viewFocus = Map.fromList
    [ ( ChessboardView, focusRing [Chessboard] )
    , ( ConfigEditorView, focusRing [ConfigEditor] )
    ]
  edit = editor ConfigEditor Nothing (Text.decodeUtf8 $(embedFile "app/Main.hs"))

data Name = ConfigEditor | RebuildError
          | Chessboard
          deriving (Eq, Ord, Read, Show)

type AppEvent = [UCI.Info]

focusedView :: Getter AppState (Maybe ViewName)
focusedView = asFocusL . to focusGetCurrent

setView :: ViewName -> AppState -> AppState
setView v = asFocusL %~ focusSetCurrent v

viewName :: AppState -> ViewName
viewName = fromMaybe ConfigEditorView . view focusedView

focusedWidget :: AppState -> Maybe (FocusRing Name)
focusedWidget s = s ^. asViewFocusL . at (viewName s)

app :: Brick.App AppState AppEvent Name
app = Brick.App { .. } where
  appStartEvent = execStateT $ reloadConfigFile False >> join (use startupAction)
  appDraw s = renderView (viewName s) s
  appHandleEvent s = go where
    go (VtyEvent e) = handleViewEvent (viewName s) s e
    go (AppEvent i) = case (find isScore i, find isPV i) of
      (Just score, Just (UCI.PV pv)) -> Brick.continue $
        s & asAnalyser . traverse . aPV .~ Just pv
      _ -> Brick.continue s
    go _            = Brick.continue s
  appAttrMap = view themeL
  appChooseCursor s
    | s ^. asViewFocusL . at (viewName s) . to isJust
    = focusRingCursor (fromJust . (^. (asViewFocusL . at (viewName s)))) s
    | otherwise
    = Brick.neverShowCursor s

isScore UCI.Score{} = True
isScore _ = False
isPV UCI.PV{} = True
isPV _ = False

main :: Config -> IO ()
main inCfg = do
  cfg <- execParser $ info (commandLine inCfg <**> helper) $ briefDesc
  chan <- newBChan (channelSize cfg)
  let buildVty = Vty.mkVty Vty.defaultConfig
  vty <- buildVty
  appState <- Brick.customMain vty buildVty (Just chan)
              app (initialState chan cfg)
  when (appState ^. asRelaunch) $
    Dyre.relaunchMaster Nothing

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
