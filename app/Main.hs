{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Hydra.Prelude hiding       (putStrLn, getLine, putStr)
import qualified Hydra.Prelude as P
import           System.Environment         (getArgs)
import qualified Data.Text                  as T
import qualified Data.Map                   as Map
import           Data.Data
import           Data.Generics.Product.Fields
import           GHC.Generics
import qualified GHC.Generics               as GS
import           System.Console.Haskeline
import           System.Console.Haskeline.History

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L
import           Hydra.Language
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

putStrLn :: Text -> L.LangL ()
putStrLn = L.evalIO . P.putStrLn

-- std handlers:
-- ping :: Ping -> L.NodeL Text
-- getBlock :: GetBlock -> L.NodeL Text
-- interface definition:
    -- L.stdF (completeWith cliCommands) $ do
    --     L.stdHandler ping
    --     L.stdHandler getBlock
-- CLI completion
-- completeWith :: [L.CLICommand] -> String -> [Completion]
-- completeWith possibles left = case filter (=~ left) possibles of
--   []  -> []
--   [x] -> [Completion x x False]
--   xs  -> map (\str -> Completion left str False) xs
-- Language; stdHandler:
-- data CmdHandlerF a where
--     CmdHandler :: Text -> CmdHandler -> (() -> a)  -> CmdHandlerF a
-- type CmdHandler    = String -> L.NodeL Text
-- type CmdHandlerL a = Free CmdHandlerF a
-- Commands ([TypeRep])
-- docHelp = typeOf Ping
--     : typeOf StopRequest
--     -- local activity
--     : typeOf Help
--     : typeOf M.CreateNodeId
--     : []

readTemplates :: St -> L.LangL Templates
readTemplates st = L.readVarIO $ st ^. _templates

readPlayers :: St -> L.LangL Players
readPlayers st = L.readVarIO $ st ^. _players

showTemplates :: St -> L.LangL Text
showTemplates st = do
  tsMap <- readTemplates st
  pure $ "==== Templates:\n" +|| Map.size tsMap ||+ ""

showPlayers :: St -> L.LangL Text
showPlayers st = do
  psMap <- readPlayers st
  pure $ "==== Players:\n" +|| Map.size psMap ||+ ""


data Loop
  = Continue
  | Finish

data Template = Template

data Player = Player

type Templates = Map.Map Text (D.StateVar Template)
type Players = Map.Map Text (D.StateVar Player)

data St = St
  { templates :: D.StateVar Templates
  , players   :: D.StateVar Players
  }
  deriving (Generic)

_templates :: HasField' "templates" s a => Lens s s a a
_templates = field' @"templates"

_players :: HasField' "players" s a => Lens s s a a
_players = field' @"players"

data ShowTemplates = ShowTemplates
  deriving (Generic, Show, Read, Eq)

data ShowPlayers = ShowPlayers
  deriving (Generic, Show, Read, Eq)

-- addLocation :: St -> Location -> L.LangL Text
-- addLocation loc = pure ""
--
--
-- addCharacter :: St -> Character -> L.LangL Text
-- addCharacter ch = pure ""


data Location = Location
  { name :: String
  }
  deriving (Generic, Typeable, Data)

data Character = Character
  { name :: String
  , age :: Int
  }
  deriving (Generic, Typeable, Data)

data FieldType
  = FStr
  | FInt
  deriving (Show)

data FieldDef = FieldDef String FieldType
  deriving (Show)

class ToFieldDef1 f where
  toFieldDef1 :: f p -> [FieldDef]

instance (ToFieldDef1 f) => ToFieldDef1 (GS.M1 D t f) where
  toFieldDef1 _ = toFieldDef1 (Proxy :: Proxy f)

instance (ToFieldDef1 s1, Constructor c) => ToFieldDef1 (GS.C1 c f) where
  -- toFieldDef1 _ = [conName (undefined :: C1 c f g)]
  toFieldDef1 _ = []


-- instance ToFieldDef1 cons => ToFieldDef1 (GS.D1 meta cons) where
--   toFieldDef1 (GS.M1 d) = toFieldDef1 d

-- instance ToFieldDef1 s1 => ToFieldDef1 (GS.C1 meta s1) where
--   toFieldDef1 (GS.M1 c) = toFieldDef1 c

instance ToFieldDef1 t => ToFieldDef1 (GS.S1 meta t) where
  toFieldDef1 (GS.M1 s) = toFieldDef1 s

instance ToFieldDef1 (GS.K1 R String) where
  toFieldDef1 (GS.K1 k) = [FieldDef "" FStr]

instance ToFieldDef1 (GS.K1 R Int) where
  toFieldDef1 (GS.K1 k) = [FieldDef "" FInt]

instance
  (ToFieldDef1 s1_1, ToFieldDef1 s1_2)
  => ToFieldDef1 ((:*:) s1_1 s1_2) where
  toFieldDef1 ((:*:) s1 s2) = toFieldDef1 s1 ++ toFieldDef1 s2

defaultToFieldDef :: (Generic a, ToFieldDef1 (Rep a)) => a -> [FieldDef]
defaultToFieldDef = toFieldDef1 . from

userCmd :: Generic a => String -> (a -> L.LangL Text) -> CmdHandlerL ()
userCmd = undefined

showTemplatesH :: St -> ShowTemplates -> L.LangL Text
showTemplatesH st _ = showTemplates st

showPlayersH :: St -> ShowPlayers -> L.LangL Text
showPlayersH st _ = showPlayers st

mainLoop :: St -> AppL ()
mainLoop st = L.std $ do
  L.stdHandler (showTemplatesH st)
  L.stdHandler (showPlayersH st)
  --
  -- L.userCmd_ "show templates" $ showTemplates st
  -- L.userCmd "add location" @Location $ addLocation st
  -- L.userCmd "add location" $ addLocation st
  -- L.userCmd "add character" @Character $ addCharacter st
  -- L.userCmd "add character" $ addCharacter st

app :: AppL ()
app = do
  L.scenario $ putStrLn "Yellow Stone: Master's Assistant Tool."

  st <- St
    <$> newVarIO Map.empty
    <*> newVarIO Map.empty

  mainLoop st

  L.awaitAppForever




loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = True
  , D._logToFile    = False
  }


main :: IO ()
main = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt app)
