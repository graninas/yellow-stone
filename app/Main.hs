{-# LANGUAGE BangPatterns #-}

module Main where

import           Hydra.Prelude hiding       (putStrLn, getLine, putStr)
import qualified Hydra.Prelude as P
import           System.Environment         (getArgs)
import qualified Data.Text                  as T
import qualified Data.Map                   as Map
import           Data.Generics.Product.Fields
import           System.Console.Haskeline
import           System.Console.Haskeline.History

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L
import           Hydra.Language
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

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
-- stdHandler :: (Typeable a, Read a) => (a -> L.NodeL Text) -> CmdHandlerL ()
-- stdHandler f = liftF $ CmdHandler (D.toTag f) (makeStdHandler f) id
--
-- haskeline usage:
-- import           System.Console.Haskeline
-- import           System.Console.Haskeline.History
--
-- callHandler :: NodeRuntime -> Map Text (String -> L.NodeL Text) -> String -> IO Text
-- callHandler nodeRt methods msg = do
--     let tag = T.pack $ takeWhile (/= ' ') msg
--     case methods ^. at tag of
--         Just justMethod -> Impl.runNodeL nodeRt $ justMethod msg
--         Nothing         -> pure $ "The method " <> tag <> " isn't supported."
--
-- interpretNodeDefinitionL nodeRt (L.Std handlers next) = do
--     m <- atomically $ newTVar mempty
--     _ <- runCmdHandlerL m handlers
--     void $ forkIO $ do
--         m'       <- readTVarIO m
--         tag      <- readTVarIO (nodeRt ^. RLens.nodeTag)
--         let
--             filePath = nodeRt ^. RLens.storyPaths.at tag
--             inpStr = if tag == "Client" then "λ> " else ""
--             loop   = do
--                 minput <- getInputLine inpStr
--                 case minput of
--                     Nothing      -> pure ()
--                     Just    line -> do
--                         res <- liftIO $ callHandler nodeRt m' line
--                         outputStrLn $ T.unpack res
--                         whenJust filePath $ \path -> do
--                             history <- getHistory
--                             liftIO $ writeHistory path history
--                         loop
--     runInputT defaultSettings{historyFile = filePath} loop

putStrLn :: Text -> AppL ()
putStrLn = evalIO . P.putStrLn

-- putStr :: Text -> AppL ()
-- putStr t = evalIO (P.putStr t >>= pure)

getLine :: AppL Text
getLine = evalIO P.getLine

getUserInput :: AppL Text
getUserInput = getLine


decayInput :: Text -> [Text]
decayInput = T.words


readTemplates :: St -> AppL Templates
readTemplates st = L.readVarIO $ st ^. _templates

readPlayers :: St -> AppL Players
readPlayers st = L.readVarIO $ st ^. _players

showTemplates :: St -> AppL ()
showTemplates st = do
  putStrLn "==== Templates:"
  tsMap <- readTemplates st
  putStrLn $ "(tmp) " +|| Map.size tsMap ||+ ""

showPlayers :: St -> AppL ()
showPlayers st = do
  putStrLn "==== Players:"
  psMap <- readPlayers st
  putStrLn $ "(tmp) " +|| Map.size psMap ||+ ""


data Loop
  = Continue
  | Finish


mainLoop :: St -> AppL ()
mainLoop st = do
  putStrLn ""
  input <- getUserInput

  res <- case decayInput input of
    []                      -> pure Continue
    ("show": "templates":_) -> showTemplates st *> pure Continue
    ("show": "players":_)   -> showPlayers st   *> pure Continue
    _                       -> pure Continue

  case res of
    Continue -> mainLoop st
    Finish   -> pure ()


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

app :: AppL ()
app = do
  putStrLn "Yellow Stone: Master's Assistant Tool."

  st <- St
    <$> newVarIO Map.empty
    <*> newVarIO Map.empty

  mainLoop st


  putStrLn "Done."




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
