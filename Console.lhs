> {-# LANGUAGE DeriveDataTypeable  #-}
> module Console where
> import Twiz
> import qualified Control.Monad.State as S
> import Text.JSON.Generic (Data, Typeable)
> import System.IO
> import System.Exit

-------------------------------------------------------------------------------
Some Useful routines. (Depends on the existance of MyState unlike Common)
-------------------------------------------------------------------------------

> data MyState = G {socket :: Handle, db::Db, users::UserMap}
>   deriving (Typeable,Data)

> type Con = S.StateT MyState IO

> io :: IO a -> Con a
> io = S.liftIO

> puts :: String -> Con ()
> puts = io . putStrLn

> gets :: Con String
> gets = io getLine

> put :: String -> Con ()
> put = io . putStr

> putA = foldr ((>>) . puts) (return ())

> bye = io $ exitWith ExitSuccess

