{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
-- {-# LANGUAGE UndecidableInstances        #-}
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
import           Data.Default
import           GHC.Generics
import qualified GHC.Generics               as GS
import           System.Console.Haskeline
import           System.Console.Haskeline.History

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L
import           Hydra.Language
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R
import Unsafe.Coerce


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


putStrLn :: Text -> L.LangL ()
putStrLn = L.evalIO . P.putStrLn

readTemplates :: St -> L.LangL Templates
readTemplates st = L.readVarIO $ st ^. _templates

readPlayers :: St -> L.LangL Players
readPlayers st = L.readVarIO $ st ^. _players

showTemplates :: St -> L.LangL (Maybe String)
showTemplates st = do
  tsMap <- readTemplates st
  pure $ Just $ "==== Templates:\n" +|| Map.size tsMap ||+ ""

showPlayers :: St -> L.LangL (Maybe String)
showPlayers st = do
  psMap <- readPlayers st
  pure $ Just $ "==== Players:\n" +|| Map.size psMap ||+ ""

addLocation :: St -> Location -> L.LangL (Maybe String)
addLocation st loc = pure $ Just "addLocation"


type Name = String
data Location = Location Name
  deriving (Generic, Typeable, Data, Default, Show, Read, Eq)


mainLoop :: St -> AppL ()
mainLoop st = L.std $ do
  L.simpleCmd "show templates" $ showTemplates st
  L.userCmd   "add location"   $ addLocation st

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
