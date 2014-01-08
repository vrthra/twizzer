> module Db where
> import qualified Control.Monad.State as S
> import Console

A very tiny class for future when we have big databases to support.

> class Persist a where
>   saveState :: a -> Con ()
>   restoreState :: a -> Con ()

