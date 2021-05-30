module Pure.Control.Cont (Cont,runCont,reset,shift) where

import Pure.Elm hiding (Shift,shift)

import Data.Typeable

data ContC a = ContC
  { reset_ :: (a -> View) -> IO ()
  , shift_ :: a -> IO ()
  }

type Cont a = ?cont :: ContC a

data Msg a
  = Reset (a -> View)
  | Shift a

data Model a = Model (Maybe a) (Cont a => a -> View)

data Env a = Env (Cont a => View)

runCont :: Typeable a => (Cont a => View) -> (a -> View) -> View
runCont v f = run (App [] [] [] (pure (Model Nothing f)) update view) (Env v)

update :: Msg a -> Env a -> Model a -> IO (Model a)
update msg _ (Model ma f) =
  case msg of
    Reset f' -> pure (Model Nothing f')
    Shift a  -> pure (Model (Just a) f)

view :: Elm (Msg a) => Env a -> Model a -> View
view (Env v) (Model ma f)
  | Nothing <- ma = let ?cont = ContC (command . Reset) (command . Shift) in v
  | Just a  <- ma = let ?cont = ContC (command . Reset) (command . Shift) in f a

reset :: Cont a => (a -> View) -> IO ()
reset = reset_ ?cont

shift :: Cont a => a -> IO ()
shift = shift_ ?cont
