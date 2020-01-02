module Pure.Control.Except (Except,catch,throw) where

import Pure.Elm

import Data.Typeable

data ExceptC e = ExceptC
  { throw_ :: e -> IO ()
  }

type Except e = ?except :: ExceptC e

data Msg e = Throw e

data Model e = Model (Maybe e)

data Env e = Env (Except e => View) (e -> View)

catch :: Typeable e => (Except e => View) -> (e -> View) -> View
catch f h = run (App [] [] [] (Model Nothing) update view) (Env f h)

update :: Msg e -> Env e -> Model e -> IO (Model e)
update msg _ _ =
  case msg of
    Throw e -> pure (Model (Just e))

view :: Elm (Msg e) => Env e -> Model e -> View
view (Env f h) (Model me)
  | Just e <- me = h e
  | otherwise    = let ?except = ErrorC (command . Throw) in f

throw :: Except e => e -> IO ()
throw = throw_ ?except