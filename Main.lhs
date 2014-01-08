> {-# LANGUAGE TypeSynonymInstances,FlexibleContexts #-}
> module Main where

> import Data.List
> import Data.Typeable
> import System.IO
> import System.Exit
> import Control.Monad (forever)
> import qualified Control.Exception as E
> import qualified Control.Monad.State as S

> import Interact
> import Twiz
> import Cmd

> type ConIO = Con IO

> instance Proto ConIO where
>   puts s = io $ putStrLn s
>   put s = io $ putStr s
>   gets = io $ getLine 
>   putA [] = return ()
>   putA (s:ss) = puts s >> putA ss
>   quit =
>       do s <- S.get
>          io $ save s
>          io $ (exitWith ExitSuccess)
>   prompt = put "> "
>   banner =
>       do io $ hSetBuffering stdout NoBuffering
>          state <- io $ restore
>          S.put state
>          puts "Twizer: for help at any time, !help"

> io :: IO a -> Con IO a
> io = S.liftIO

Some uninteresting IO routines.

> run :: (Monad m, Proto (Con m))  => Con m ()
> run = banner >> forever repl

Our point of entry. We acquire the required resources (FD, DB connection, network..)
And release it at the end of use.

> mymain :: IO ()
> mymain = E.bracket acquire return process
>   where acquire = return $ G [] []

We wrap the stuff in a stateT monad, So now we have a place to keep our global state.

> process :: MyState -> IO ()
> process st = S.runStateT run st >> return ()

> tinit :: ConIO ()
> tinit = do createUsers ["u0", "u1","u2","u3","u4", "u4", "u5", "u6", "u7", "u8", "u9"]
>            createTwiz "one" "1 2 3"
>            createTwiz "two" "2 3 4"
>            assignBuddy "one" "u1" ["u2","u3"]
>            assignBuddy "one" "u2" ["u3","u4"]
>            assignBuddy "one" "u3" ["u4","u5"]


