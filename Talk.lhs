> module Talk where
> import qualified Control.Monad.State as S

> import Console

> class Talk a where
>   initializeTalk :: a -> Con ()
>   send :: a -> String -> Con ()
>   sendA :: a -> [String] -> Con ()
>   recv :: a -> Con String

