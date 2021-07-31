{-# LANGUAGE DeriveGeneric        #-}
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
{-# LANGUAGE ViewPatterns         #-}
module Annalise (
  Config( engineExecutable
        , engineArgs
        , channelSize
        , theme
        , chessboardKeymap
        , configEditorKeymap
        , globalKeymap
        , onStartup
        , defaultView
        )
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
import           Brick.Types                  (Padding(..), BrickEvent (AppEvent, VtyEvent),
                                               EventM, Location (..), Next,
                                               ViewportType (Both, Horizontal, Vertical),
                                               Widget, handleEventLensed)
import           Brick.Widgets.Border         (border, borderWithLabel)
import           Brick.Widgets.Center         (hCenter)
import           Brick.Widgets.Chess
import           Brick.Widgets.Core           (padTop, emptyWidget, padBottom, padLeft, padRight, fill, hBox, hLimit, showCursor,
                                               str, strWrap, txt, txtWrap, vBox,
                                               vLimit, viewport, (<+>), (<=>))
import           Brick.Widgets.Edit           (Editor, editContentsL, editor,
                                               getEditContents,
                                               handleEditorEvent, renderEditor)
import qualified Brick.Widgets.List           as Brick
import           Brick.Widgets.Skylighting    (attrMappingsForStyle, highlight)
import qualified Config.Dyre                  as Dyre
import qualified Config.Dyre.Paths            as Dyre
import qualified Config.Dyre.Relaunch         as Dyre
import           Control.Applicative          ((<|>))
import           Control.Concurrent           (ThreadId, forkIO, killThread)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Lens                 (Getter, Getting, Lens', assign,
                                               at, from, ix, lens, modifying,
                                               to, use, uses, view, (%=), (%~),
                                               (&), (.=), (.~), (<>=), (?~),
                                               (^.), (^?!), (^?))
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
import           Data.Foldable                (foldl', for_)
import           Data.Functor                 ((<&>))
import           Data.List                    (elemIndex, find, intersperse,
                                               isPrefixOf, sort)
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, fromMaybe, isJust)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Zipper             as Zipper
import           Data.Tree                    (Tree (..), foldTree)
import           Data.Tree.Zipper             (Full, TreePos, forest,
                                               fromForest, label, nextTree)
import qualified Data.Tree.Zipper             as TreePos
import qualified Data.Vector                  as Vector
import qualified Data.Vector.Unboxed          as Unboxed
import           GHC.Generics                 (Generic)
import           Game.Chess
import           Game.Chess.Polyglot
import           Game.Chess.SAN
import qualified Game.Chess.UCI               as UCI
import qualified Graphics.Vty                 as Vty
import           Options.Applicative          hiding (str)
import           Paths_annalise               (getLibDir)
import           Skylighting
import           System.Directory             (doesFileExist)
import           Time.Rational                (KnownDivRat)
import           Time.Units                   (Microsecond, Time, sec)

data Config = Config
  { engineExecutable   :: FilePath
  , engineArgs         :: [String]
  , channelSize        :: Int
  , theme              :: Brick.AttrMap
  , helpKeymap :: Keymap
  , chessboardKeymap   :: Keymap
  , explorerKeymap     :: Keymap
  , configEditorKeymap :: Keymap
  , globalKeymap       :: Keymap
  , onStartup          :: Action ()
  , defaultView        :: ViewName
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

helpKeymapL :: Lens' Config Keymap
helpKeymapL = lens helpKeymap $ \s b -> s { helpKeymap = b }

class HasChessboardKeymap a where chessboardKeymapL :: Lens' a Keymap
instance HasChessboardKeymap Config where
  chessboardKeymapL = lens chessboardKeymap $ \s b -> s { chessboardKeymap = b }

class HasExplorerKeymap a where explorerKeymapL :: Lens' a Keymap
instance HasExplorerKeymap Config where
  explorerKeymapL = lens explorerKeymap $ \s b -> s { explorerKeymap = b }

class HasConfigEditorKeymap a where configEditorKeymapL :: Lens' a Keymap
instance HasConfigEditorKeymap Config where
  configEditorKeymapL = lens configEditorKeymap $ \s b ->
    s { configEditorKeymap = b }

class HasGlobalKeymap a where globalKeymapL :: Lens' a Keymap
instance HasGlobalKeymap Config where
  globalKeymapL = lens globalKeymap $ \s b -> s { globalKeymap = b }

class HasStartupAction a where startupAction :: Getter a (Action ())
instance HasStartupAction Config where
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
  channelSize = 20
  theme = defaultTheme breezeDark
  helpKeymap = defaultHelpKeymap
  chessboardKeymap = defaultChessboardKeymap
  explorerKeymap = defaultExplorerKeymap
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
  option auto (long "channel-size"
            <> metavar "INT"
            <> help "Event channel size"
            <> showDefault <> value (channelSize cfg)) <*>
  pure (theme cfg) <*>
  pure (helpKeymap cfg) <*>
  pure (chessboardKeymap cfg) <*>
  pure (explorerKeymap cfg) <*>
  pure (configEditorKeymap cfg) <*>
  pure (globalKeymap cfg) <*>
  pure (onStartup cfg) <*>
  pure (defaultView cfg) <*>
  pure (configFile cfg) <*>
  pure (dyreError cfg)

------------------------------------------------------------------------------

renderHelp s = [ui] where
  ui = header <=> vp where
    header = border $ padLeft (Pad 3) $ padRight (Pad 3) $ hCenter $ txt "annalise help"
    vp = viewport Help Vertical content
    content = vBox $ padTop (Pad 1) . r <$>
      [ s ^. asConfig . globalKeymapL
      , s ^. asConfig . chessboardKeymapL
      , s ^. asConfig . explorerKeymapL
      , s ^. asConfig . configEditorKeymapL
      ]
    r = Map.foldlWithKey f emptyWidget where
      f w k (d, _) = w <=> ((vLimit 1 . hLimit 20 $ txt (ppVtyEvent k) <+> fill ' ') <+> txtWrap d)

ppVtyEvent :: Vty.Event -> Text
ppVtyEvent (Vty.EvKey k mods) = Text.intercalate "-" $
  (ppMod <$> mods) <> [ppKey k]

ppKey (Vty.KChar c) = Text.singleton c
ppKey (Vty.KFun n) = Text.pack $ "<F" <> show n <> ">"
ppKey Vty.KRight = "<Right>"
ppKey Vty.KLeft = "<Left"
ppKey Vty.KUp = "<Up>"
ppKey Vty.KDown = "<Down>"
ppKey Vty.KEnter = "<Enter>"
ppKey Vty.KBS = "<Backspace>"
ppKey Vty.KEsc = "ESC"

ppMod Vty.MMeta = "M"
ppMod Vty.MCtrl = "C"

helpUp, helpDown :: Action ()
helpUp = lift $ Brick.vScrollBy (Brick.viewportScroll Help) (-1)
helpDown = lift $ Brick.vScrollBy (Brick.viewportScroll Help) 1

defaultHelpKeymap :: Keymap
defaultHelpKeymap = Map.fromList
  [ ( Vty.EvKey Vty.KUp [], ("Scroll up", helpUp *> continue) )
  , ( Vty.EvKey Vty.KDown [], ("Scroll down", helpDown *> continue) )
  ]

helpHandler :: EventHandler
helpHandler = EventHandler (asConfig . helpKeymapL) $ \e ->
  continue

----------------------------------------------------------------------------------

data Explorer = Explorer
  { _eInitial :: Position
  , _eTreePos :: TreePos Full (NonEmpty Ply)
  }

eInitial :: Lens' Explorer Position
eInitial = lens _eInitial $ \s b -> s { _eInitial = b }

eTreePos :: Lens' Explorer (TreePos Full (NonEmpty Ply))
eTreePos = lens _eTreePos $ \s b -> s { _eTreePos = b }

defaultExplorer :: Explorer
defaultExplorer = Explorer { .. } where
  _eInitial = startpos
  _eTreePos = fromJust . nextTree . fromForest $
              pathTree <$> bookForest defaultBook _eInitial

pathTree :: Tree a -> Tree (NonEmpty a)
pathTree = foldTree $ \a -> Node (pure a) . (fmap . fmap) (NonEmpty.cons a)

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
  go e = [ui] where
    ui = (hLimit 9 list <+> hLimit 23 board <+> var)
     <=> fill ' '
     <=> renderMessage s
    list = withFocusRing' s ExplorerView
      (Brick.renderList (drawPly (ePreviousPosition e))) (ePlyList e)
    drawPly p foc = putCursorIf foc PlyList (0, 0) . str . toSAN p
    board = renderPosition Chessboard (eCurrentPosition e) (Just (color (ePreviousPosition e))) Nothing english True
    var = strWrap $ varToSAN (e ^. eInitial) (e ^. eTreePos . to label)

withFocusRing' s v f a = withFocusRing (s ^?! asViewFocus . ix v)
  f a

explorerHandler :: EventHandler
explorerHandler = EventHandler (asConfig . explorerKeymapL) $ \e -> do
  message $ "Unbound key: " <> show e
  continue

explorerPrev, explorerNext, explorerFirstChild, explorerParent :: Action ()
explorerPrev = asExplorer . eTreePos %= (fromMaybe <*> TreePos.prev)
explorerNext = asExplorer . eTreePos %= (fromMaybe <*> TreePos.next)
explorerFirstChild = asExplorer . eTreePos %= (fromMaybe <*> TreePos.firstChild)
explorerParent = asExplorer . eTreePos %= (fromMaybe <*> TreePos.parent)

defaultExplorerKeymap = Map.fromList
  [ ( Vty.EvKey Vty.KDown []
    , ("Select next move", explorerNext *> continue)
    )
  , ( Vty.EvKey Vty.KUp []
    , ("Select previous move", explorerPrev *> continue)
    )
  , ( Vty.EvKey Vty.KRight []
    , ("First child", explorerFirstChild *> continue)
    )
  , ( Vty.EvKey Vty.KLeft []
    , ("Parent", explorerParent *> continue)
    )
  ]

------------------------------------------------------------------------------

data Analyser = Analyser
  { _aEngine :: UCI.Engine
  , _aReader :: Maybe (TChan UCI.BestMove, ThreadId)
  , _aPV     :: Maybe (Unboxed.Vector Ply)
  }

aEngine :: Lens' Analyser UCI.Engine
aEngine = lens _aEngine $ \s b -> s { _aEngine = b }

aReader :: Lens' Analyser (Maybe (TChan UCI.BestMove, ThreadId))
aReader = lens _aReader $ \s b -> s { _aReader = b }

aPV :: Lens' Analyser (Maybe (Unboxed.Vector Ply))
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
      asAnalyser .= Just (a & aReader .~ Just (bmc, tid))
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
        asAnalyser . traverse . aPV     .= Nothing
        pure True
      Nothing -> pure False
    Nothing -> pure False

------------------------------------------------------------------------------

data ViewName = HelpView
              | ChessboardView
              | ExplorerView
              | ConfigEditorView
              deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Binary ViewName

type Action = StateT AppState (EventM Name)
type Keymap = Map Vty.Event (Text, Action (Next AppState))

haskellSyntax :: Syntax
haskellSyntax = fromJust $ "haskell" `lookupSyntax` defaultSyntaxMap

renderView :: ViewName -> AppState -> [Widget Name]
renderView HelpView = renderHelp
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
    , ("Quit the application", quit)
    )
  , ( Vty.EvKey (Vty.KFun 1) []
    , ("Help view", switchView HelpView *> continue)
    )
  , ( Vty.EvKey (Vty.KFun 2) []
    , ("Game view", switchView ChessboardView *> continue)
    )
  , ( Vty.EvKey (Vty.KFun 3) []
    , ("Book explorer", switchView ExplorerView *> continue)
    )
  , ( Vty.EvKey (Vty.KFun 10) []
    , ("Configuration editor", switchView ConfigEditorView *> continue)
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
relaunch = do
  asRelaunch .= True
  quit

quit :: Action (Next AppState)
quit = get >>= lift . Brick.halt

defaultConfigEditorKeymap :: Keymap
defaultConfigEditorKeymap = Map.fromList
  [ ( Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]
    , ("Save and relaunch", writeConfigFile *> relaunch)
    )
  , ( Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]
    , ("Write configuration to disk", writeConfigFile *> continue)
    )
  , ( Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]
    , ("Revert", reloadConfigFile True *> continue)
    )
  , ( Vty.EvKey (Vty.KChar '<') [Vty.MMeta]
    , ("Go to beginning of file", gotoBeginningOfConfig *> continue)
    )
  , ( Vty.EvKey (Vty.KChar '>') [Vty.MMeta]
    , ("Go to end o f file", gotoEndOfConfig *> continue)
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
  ([ ( Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl]
     , ("Toggle analyser", toggleAnalyser *> continue)
     )
   , ( Vty.EvKey Vty.KLeft [],
       ("Move one square to the left", cursorLeft *> continue ))
   , ( Vty.EvKey Vty.KRight []
     , ("Move one square to the right", cursorRight *> continue)
     )
   , ( Vty.EvKey Vty.KUp []
     , ("Move one square up", cursorUp *> continue)
     )
   , ( Vty.EvKey Vty.KDown []
     , ("Move one square down", cursorDown *> continue)
     )
   , ( Vty.EvKey Vty.KEnter []
     , ("Select current square", enter *> continue)
     )
   , ( Vty.EvKey Vty.KBS []
     , ("Delete ply input backwards", plyInputBS *> continue)
     )
   , ( Vty.EvKey Vty.KEsc []
     , ("Clear ply input", clearInput *> continue)
     )
   , ( Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]
     , ("Reset game", resetGame *> continue)
     )
   ] <>
  ) $
  "abcdefgh12345678knqrxo" <&> \c ->
  ( Vty.EvKey (Vty.KChar c) []
  , ("Ply input", plyInput c *> continue)
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
    Just (_, a)  -> a
    Nothing -> fallback e

handleViewEvent :: ViewName -> AppState -> Vty.Event -> EventM Name (Next AppState)
handleViewEvent HelpView = dispatch helpHandler
handleViewEvent ChessboardView   = dispatch chessboardHandler
handleViewEvent ExplorerView     = dispatch explorerHandler
handleViewEvent ConfigEditorView = dispatch configEditorHandler

------------------------------------------------------------------------------

data Game = Game
  { _gInitial :: Position
  , _gPlies   :: [Ply]
  , _gInput   :: String
  , _gFrom    :: Maybe Square
  , _gCursor  :: Square
  }

gInitial :: Lens' Game Position
gInitial = lens _gInitial $ \s b -> s { _gInitial = b }

gPlies :: Lens' Game [Ply]
gPlies = lens _gPlies $ \s b -> s { _gPlies = b }

gInput :: Lens' Game String
gInput = lens _gInput $ \s b -> s { _gInput = b }

gFrom :: Lens' Game (Maybe Square)
gFrom = lens _gFrom $ \s b -> s { _gFrom = b }

gCursor :: Lens' Game Square
gCursor = lens _gCursor $ \s b -> s { _gCursor = b }

newGame :: Game
newGame = Game startpos [] "" Nothing E1

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
        dst <- use (asGame . gCursor)
        let p pl = plyTarget pl == dst
        plies <- legalPlies <$> uses asGame currentPosition
        case filter p plies of
          [pl] -> addPly pl
          _ -> assign (asGame . gFrom) $ Just dst
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
  pv = case s ^? asAnalyser . traverse . aPV of
    Just (Just pv) -> str $ varToSAN (s ^. asGame . to currentPosition) pv
    _              -> str ""
  w g = (hLimit 23 (hCenter board) <+> var)
    <=> (hLimit 21 . vLimit 1 $ sideToMove <+> fill ' ' <+> lastPly)
    <=> input
    <=> pv
    <=> fill ' '
    <=> status
   where
    board = renderPosition Chessboard pos Nothing cursor english (null $ g ^. gInput)
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

instance HasTheme AppState where themeL = asConfig . themeL
instance HasConfigFile AppState where configFileL = asConfig . configFileL
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
  AppState chan cfg focus viewFocus newGame Nothing defaultExplorer edit Nothing False
 where
  focus = focusSetCurrent (defaultView cfg) (focusRing [minBound .. maxBound])
  viewFocus = Map.fromList
    [ ( ChessboardView, focusRing [Chessboard] )
    , ( ExplorerView, focusRing [PlyList] )
    , ( ConfigEditorView, focusRing [ConfigEditor] )
    ]
  edit = editor ConfigEditor Nothing (Text.decodeUtf8 $(embedFile "app/Main.hs"))

data Name = Help
          | Chessboard | PlyList
          | ConfigEditor | RebuildError
          deriving (Eq, Ord, Read, Show)

type AppEvent = [UCI.Info]

focusedView :: Getter AppState (Maybe ViewName)
focusedView = asFocusL . to focusGetCurrent

setView :: ViewName -> AppState -> AppState
setView v = asFocusL %~ focusSetCurrent v

viewName :: AppState -> ViewName
viewName s = fromMaybe (s ^. asConfig . defaultViewL) $ s ^. focusedView

focusedWidget :: AppState -> Maybe (FocusRing Name)
focusedWidget s = s ^. asViewFocus . at (viewName s)

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
    | isJust $ s ^? asViewFocus . ix (viewName s)
    = focusRingCursor (^?! asViewFocus . ix (viewName s)) s
    | otherwise
    = Brick.neverShowCursor s

isScore UCI.Score{} = True
isScore _           = False
isPV UCI.PV{} = True
isPV _        = False

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
