module Pure.Control.Reader (Reader,runReader,ask,local,reader) where

import Pure.Elm hiding (ask)

import Data.Typeable

data ReaderC r = ReaderC
  { ask_ :: r
  }

type Reader r = ?reader :: ReaderC r

data Env r = Env (Reader r => View) r

data Msg

data Model = Model

runReader :: Typeable r => (Reader r => View) -> r -> View
runReader v a = run (App [] [] [] (pure Model) update view) (Env v a)

update :: Msg -> Env r -> Model -> IO Model
update _ _ = pure

view :: Env r -> Model -> View
view (Env v a) _ = let ?reader = ReaderC a in v

ask :: Reader r => r
ask = ask_ ?reader

local :: (r -> r) -> (Reader r => x) -> (Reader r => x)
local f v = let ?reader = ReaderC (f ask) in v

reader :: Reader r => (r -> a) -> a
reader = ($ ask)
