module Pure.Control.Writer (Writer,runWriter,tell,hear,listen) where

import Pure.Elm

import Data.Typeable

data WriterC w = WriterC
  { tell_   :: w -> IO ()
  , hear_   :: w
  }

type Writer w = ?writer :: WriterC w

data Msg w = Tell w

data Model w = Model w

data Env w = Env (Writer w => View)

runWriter :: (Monoid w, Typeable w) => (Writer w => View) -> View
runWriter v = run (App [] [] [] (pure (Model mempty)) update view) (Env v)
  where
    update msg _ (Model x) =
      case msg of
        Tell w -> pure (Model (x <> w))

    view (Env v) (Model w) = let ?writer = WriterC (command . Tell) w in v

tell :: Writer c => c -> IO ()
tell = tell_ ?writer

hear :: Writer c => c
hear = hear_ ?writer

listen :: (Writer c, Monoid b, Typeable b) => (b -> c) -> (Writer b => View) -> View
listen f v = run (App [] [] [] (pure (Model mempty)) update view) (Env v)
  where
    proxy = tell
    update msg _ (Model x) =
      case msg of
        Tell w -> proxy (f w) >> pure (Model (x <> w))
    view (Env v) (Model w) = let ?writer = WriterC (command . Tell) w in v
