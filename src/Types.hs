{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import qualified Brick.AttrMap                as Brick
import           Brick.BChan                  (BChan)
import           Brick.Focus                  (FocusRing)
import qualified Brick.Types                  as Brick
import           Brick.Widgets.Chess
import           Brick.Widgets.HaskellEditor (HaskellEditor)
import qualified Brick.Widgets.FileBrowser    as Brick
import qualified Brick.Widgets.List           as Brick
import           Control.Concurrent           (ThreadId)
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Lens.TH              (makeLenses, makeLensesWith,
                                               makePrisms)
import           Control.Lens.TH.Suffix       (suffixRules)
import           Control.Monad.State          (StateT)
import           Data.Binary                  (Binary)
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Map                     (Map)
import           Data.Sequence                (Seq)
import           Data.Text                    (Text)
import           Data.Tree.Zipper             (Full, TreePos)
import           Data.Vector                  (Vector)
import qualified Data.Vector.Unboxed          as Unboxed
import           GHC.Generics                 (Generic)
import           Game.Chess                   (PieceType, startpos, Color, Ply, Position, Square(E1))
import qualified Game.Chess.PGN               as PGN
import qualified Game.Chess.UCI               as UCI
import qualified Graphics.Vty                 as Vty
import           Time.Units                   (Second, Time)

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

makeLenses ''Game

defaultGame :: Game
defaultGame = Game startpos [] Nothing "" Nothing E1

data PredictedVariation = PV
  { _pvScore  :: !UCI.Score
  , _pvBounds :: !(Maybe UCI.Bounds)
  , _pvPlies  :: !(Unboxed.Vector Ply)
  }

makeLenses ''PredictedVariation

data Analyser = Analyser
  { _aEngine :: UCI.Engine
  , _aReader :: Maybe (TChan UCI.BestMove, ThreadId)
  , _aPVs    :: Vector PredictedVariation
  }

makeLenses ''Analyser

data GameChooserStep = ChooseFile (Brick.FileBrowser Name)
                     | ChooseGame (Brick.FileBrowser Name, Brick.GenericList Name Seq PGN.Game)

makePrisms ''GameChooserStep

data GameChooser = GameChooser
  { _gcStep :: GameChooserStep
  }

makeLenses ''GameChooser

data Explorer = Explorer
  { _eInitial     :: Position
  , _eTreePos     :: TreePos Full (NonEmpty Ply)
  , _eGameChooser :: Maybe GameChooser
  }

makeLenses ''Explorer

type Action = Brick.EventM Name AppState

data Binding = Binding
  { bindingDescription :: Text
  , bindingGuard       :: Action Bool
  , bindingAction      :: Action ()
  }

simpleBinding :: Text -> Action () -> Binding
simpleBinding desc = Binding desc (pure True)

type Keymap = Map Vty.Event Binding

data Config = Config
  { engineExecutable     :: FilePath
  , engineArgs           :: [String]
  , engineStartupTimeout :: Time Second
  , engineMultiPV        :: Maybe Int
  , engineThreads        :: Maybe Int
  , positionRenderer     :: PositionRenderer Name
  , channelSize          :: Int
  , theme                :: Brick.AttrMap
  , pgnDirectory         :: Maybe FilePath
  , polyglotBook         :: Maybe FilePath
  , helpKeymap           :: Keymap
  , chessboardKeymap     :: Keymap
  , explorerKeymap       :: Keymap
  , pgnBrowserKeymap     :: Keymap
  , gameListKeymap       :: Keymap
  , configEditorKeymap   :: Keymap
  , globalKeymap         :: Keymap
  , onStartup            :: Action ()
  , defaultView          :: ViewName
  , configFile           :: FilePath
  , dyreError            :: Maybe String
  }

data AppState = AppState
  { _asChannel      :: BChan AppEvent
  , _asConfig       :: Config
  , _asFocus        :: FocusRing ViewName
  , _asViewFocus    :: Map ViewName (FocusRing Name)
  , _asGame         :: Game
  , _asAnalyser     :: Maybe Analyser
  , _asExplorer     :: Explorer
  , _asConfigEditor :: HaskellEditor Name
  , _asMessage      :: Maybe Text
  , _asRelaunch     :: Bool
  }

makeLensesWith suffixRules ''Config
makeLenses ''AppState

data Persistent = Persistent ViewName Game
                  deriving (Generic)

instance Binary Persistent

data Info = UnboundEvent Vty.Event
          | WroteFile FilePath
          | SeveralPiecesCanMoveHere Square Position [Ply]
          | NotAllowedToMove Color PieceType Square
          deriving (Eq, Generic, Show)

