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

addLocation :: St -> Location -> L.LangL ()
addLocation st loc = error "test!"


addCharacter :: St -> Character -> L.LangL ()
addCharacter st ch = pure ()

data NoVals = NoVals
  deriving (Generic, Typeable, Data, Default, Show, Read, Eq)

data Test = TeSt Int
  deriving (Generic, Typeable, Data, Default, Show, Read, Eq)

data Location = Location
  { name :: String
  }
  deriving (Generic, Typeable, Data, Default, Show, Read, Eq)

data Character = Character
  { name :: String
  , age :: Int
  }
  deriving (Generic, Typeable, Data, Default, Show, Read, Eq)

data FieldType
  = FStr
  | FInt
  deriving (Show)


data BrewMethod = V60
  deriving (Generic, Show, Read)

data Country = Country String
  deriving (Generic, Show, Read)

data Coffee = MkCoffee { coffeeBeans :: String
                       , coffeeOriginCountry :: Country
                       , coffeeBrewMethod :: BrewMethod
                       }
  deriving (Generic, Show, Read)



mkLocation :: Location
mkLocation =
  to
    (M1
      (M1
        (M1
          (K1 "just loc")
        )
      )
    )


mkTest :: Test
mkTest =
  to
    (M1
      (M1
        (M1
          (K1 10)
        )
      )
    )

mkNoVals :: NoVals
mkNoVals = to
      (M1
        (M1
            U1
          )
      )


mkCharacter :: Character
mkCharacter =
  to
    (M1
      (M1
        (M1 (K1  "name")
          :*:
          M1 (K1 20)
        )
      )
    )

comb = (:*:)
type Comb = (:*:)

mkCoffee :: Coffee
mkCoffee =
  to
    (M1
      (M1
        (M1 (K1 "Single Origin")
          :*:
          (M1 (K1  (Country "Rwanda"))
            :*:
            M1 (K1 V60)
          )
        )
      )
    )


mkTest2 :: Test
mkTest2 =
  to
    (M1
      (M1
        (M1 (K1 10))
      )
    )

mkCoffee2 :: Coffee
mkCoffee2 = to
    (M1
      (M1
        (comb
          (M1 (K1 "Single Origin"))
          (comb
            (M1 (K1 (Country "Rwanda")))
            (M1 (K1 V60))
          )
        )
      )
    )


class GConstr f where
  gConstr :: [String] -> Proxy f -> f

instance (GConstr f) => GConstr (GS.M1 D t f) where
  gConstr ss _ = undefined -- gConstr ss (Proxy :: Proxy f)

-- instance (GConstr f) => GConstr (GS.C1 c f) where
--   gConstr ss _ = gConstr ss (Proxy :: Proxy f)
--
-- instance (GConstr k1, Selector s) => GConstr (GS.M1 GS.S s k1) where
--   gConstr ss _ = fs'
--     where
--       FieldDef fn ft : fs = gConstr ss (Proxy :: Proxy k1)
--       fn' = GS.selName (undefined :: M1 S s k1 k)
--       fs' = FieldDef fn' ft : fs
--
-- instance GConstr (GS.K1 GS.R String) where
--   gConstr ss _ = [FieldDef "" FStr]
--
-- instance GConstr (GS.K1 GS.R Int) where
--   gConstr ss _ = [FieldDef "" FInt]
--
-- instance
--   (GConstr f, GConstr g)
--   => GConstr (Comb f g) where
--   gConstr ss _ = gConstr ss (Proxy :: Proxy f) ++ gConstr ss (Proxy :: Proxy g)

toGConstr :: forall a. (Generic a, GConstr (Rep a))
  => [String] -> Proxy a -> [FieldDef]
toGConstr ss _ = gConstr ss (Proxy :: Proxy (Rep a))

-- parseGen :: Generic a => [String] -> [FieldDef] -> a
-- -- parseGen [] [] = M1 (M1 U1)
-- parseGen (s:ss) [] = error "tokens do not match field defs"
-- parseGen [] (f:fs) = error "field defs do not match tokens"
-- parseGen ss fs = to $ M1 (M1 (mkGen' ss fs))
--   where
--     mkGen' (s:ss) [FieldDef n FInt] = M1 (K1 (parseIntF n s))
--     mkGen' (s:ss) (FieldDef n FInt : fs) =
--       comb
--         (M1 (K1 (parseIntF n s)))
--         (mkGen' ss fs)
--
--     parseIntF :: String -> String -> Int      -- No errors for now
--     parseIntF n s = case readMaybe s of
--       Nothing -> error $ "Failed to read Int field " -- ++ n ++ ": " ++ s
--       Just n  -> n

data FieldDef = FieldDef String FieldType
  deriving (Show)

class ToFieldDef1 f where
  toFieldDef1 :: Proxy f -> [FieldDef]

instance (ToFieldDef1 f) => ToFieldDef1 (GS.M1 D t f) where
  toFieldDef1 _ = toFieldDef1 (Proxy :: Proxy f)

instance (ToFieldDef1 f) => ToFieldDef1 (GS.C1 c f) where
  toFieldDef1 _ = toFieldDef1 (Proxy :: Proxy f)

instance (ToFieldDef1 k1, Selector s) => ToFieldDef1 (GS.M1 GS.S s k1) where
  toFieldDef1 _ = fs'
    where
      FieldDef fn ft : fs = toFieldDef1 (Proxy :: Proxy k1)
      fn' = GS.selName (undefined :: M1 S s k1 k)
      fs' = FieldDef fn' ft : fs

instance ToFieldDef1 (GS.K1 GS.R String) where
  toFieldDef1 _ = [FieldDef "" FStr]

instance ToFieldDef1 (GS.K1 GS.R Int) where
  toFieldDef1 _ = [FieldDef "" FInt]

instance
  (ToFieldDef1 f, ToFieldDef1 g)
  => ToFieldDef1 ((:*:) f g) where
  toFieldDef1 _ = toFieldDef1 (Proxy :: Proxy f) ++ toFieldDef1 (Proxy :: Proxy g)

toFieldDef :: forall a. (Generic a, ToFieldDef1 (Rep a))
  => Proxy a -> [FieldDef]
toFieldDef _ = toFieldDef1 (Proxy :: Proxy (Rep a))

userCmd
  :: forall a
   . (Data a, Generic a, ToFieldDef1 (Rep a), Default a)
  => String
  -> (a -> L.LangL Text)
  -> CmdHandlerL ()
userCmd cmd handler = do
  let fieldDefs = toFieldDef (Proxy :: Proxy a)


  -- let fieldConstrs = map (\(FieldDef n t) -> (n, getConstr t)) fieldDefs

  -- let enumFields = do
  --       i <- get
  --       modify (+1)
  --       let FieldDef n t = fieldDefs !! i
  --       pure $ getConstr t
  -- let v :: a = evalState (fromConstrM enumFields (toConstr (def :: a))) 0
  pure ()

  -- where
  --   getConstr FStr = fromConstr (toConstr @String "")
  --   getConstr FInt = fromConstr (toConstr @Int 0)

testCh = ("ab" :: String)

-- test :: String
-- test = evalState
--   (fromConstrM
--     (do
--       i <- get
--       modify (+1)
--       pure $ unsafeCoerce $ testCh !! i
--     )
--     (toConstr ("c" :: [Char]))
--   )
--   0

mainLoop :: St -> AppL ()
mainLoop st = L.std $ do
  -- L.userCmd_ "show templates" $ showTemplates st
  L.userCmd "add location" $ addLocation st
  L.userCmd "add character" $ addCharacter st

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
