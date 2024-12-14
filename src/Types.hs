{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE TemplateHaskell       #-}
module Types where

import qualified Brick.AttrMap                as Brick
import           Brick.BChan                  (BChan)
import           Brick.Focus                  (FocusRing)
import qualified Brick.Types                  as Brick
import           Brick.Widgets.Chess
import qualified Brick.Widgets.FileBrowser    as Brick
import           Brick.Widgets.HaskellEditor  (HaskellEditor)
import qualified Brick.Widgets.List           as Brick
import           Control.Concurrent           (ThreadId)
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Lens.TH              (makePrisms)
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
import           Game.Chess                   (Color, PieceType, Ply, Position,
                                               Square (E1), startpos)
import qualified Game.Chess.PGN               as PGN
import qualified Game.Chess.UCI               as UCI
import           GHC.Generics                 (Generic)
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
  { initial     :: Position
  , plies       :: [Ply]
  , perspective :: Maybe Color
  , input       :: String
  , from        :: Maybe Square
  , cursor      :: Square
  } deriving (Generic)

instance Binary Game

defaultGame :: Game
defaultGame = Game startpos [] Nothing "" Nothing E1

data PredictedVariation = PV
  { score  :: !UCI.Score
  , bounds :: !(Maybe UCI.Bounds)
  , plies  :: !(Unboxed.Vector Ply)
  } deriving (Generic)

data Analyser = Analyser
  { engine :: UCI.Engine
  , reader :: Maybe (TChan UCI.BestMove, ThreadId)
  , pvs    :: Vector PredictedVariation
  } deriving (Generic)

data GameChooserStep = ChooseFile (Brick.FileBrowser Name)
                     | ChooseGame (Brick.FileBrowser Name, Brick.GenericList Name Seq PGN.Game)

makePrisms ''GameChooserStep

newtype GameChooser = GameChooser
  { step :: GameChooserStep
  } deriving (Generic)

data Explorer = Explorer
  { initial     :: Position
  , treePos     :: TreePos Full (NonEmpty Ply)
  , gameChooser :: Maybe GameChooser
  } deriving (Generic)

type Action = Brick.EventM Name AppState

data Binding = Binding
  { description :: Text
  , guard       :: Action Bool
  , action      :: Action ()
  } deriving (Generic)

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
  } deriving (Generic)

data AppState = AppState
  { channel      :: BChan AppEvent
  , config       :: Config
  , focus        :: FocusRing ViewName
  , viewFocus    :: Map ViewName (FocusRing Name)
  , game         :: Game
  , analyser     :: Maybe Analyser
  , explorer     :: Explorer
  , configEditor :: HaskellEditor Name
  , message      :: Maybe Text
  , relaunch     :: Bool
  } deriving (Generic)

data Persistent = Persistent ViewName Game
                  deriving (Generic)

instance Binary Persistent

data Info = UnboundEvent Vty.Event
          | WroteFile FilePath
          | SeveralPiecesCanMoveHere Square Position [Ply]
          | NotAllowedToMove Color PieceType Square
          deriving (Eq, Generic, Show)
