> module FileDb where
> import Text.JSON as J
> import Text.JSON.Generic as JG
> import qualified Control.Monad.State as S

> import Twiz
> import Console
> import Db

We use JSON as the format of choice here. However we can create another instance of any
other format we wish.

> data Fs = Fs String

> instance Persist Fs where
>   saveState (Fs f) =
>       do (G h d u) <- S.get
>          io $ saveJSON f (d,u)
>   restoreState (Fs f) =
>       do (a',b') <- io $ catch (restoreJSON f) (\e -> return ([],[]))
>          S.modify $ \ (G h a b) -> (G h a' b')

> saveJSON f = writeFile f . J.encode . JG.toJSON
> restoreJSON f = fmap (strip . JG.fromJSON . strip . J.decode) (readFile f)
>       where strip (Ok j) = j
>             strip (Error s) = error s

