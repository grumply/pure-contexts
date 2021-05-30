module Pure.Control.State (State,runState,get,put,modify) where

import Pure.Elm hiding (get,modify)

import Data.Typeable

data StateC s = StateC
  { put_ :: s -> IO ()
  , get_ :: s
  }

type State s = ?state :: StateC s

data Msg s = Put s

data Model s = Model s

data Env s = Env (State s => View)

runState :: Typeable s => (State s => View) -> s -> View
runState v a = run (App [] [] [] (pure (Model a)) update view) (Env v)

update :: Msg s -> Env s -> Model s -> IO (Model s)
update msg _ _ =
  case msg of
    Put a -> pure (Model a)

view :: Elm (Msg s) => Env s -> Model s -> View
view (Env v) (Model a) = let ?state = StateC (command . Put) a in v

get :: State s => s
get = get_ ?state

put :: State s => s -> IO ()
put = put_ ?state

modify :: State s => (s -> s) -> IO ()
modify f = let StateC {..} = ?state in put_ (f get_)
