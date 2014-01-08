> {-# LANGUAGE TupleSections,DeriveDataTypeable #-}

> module Main where
> import System.IO
> import System (getArgs)
> import Control.Monad (forever)
> import qualified Control.Monad.State as S
> import qualified Control.Exception as E

> import Twiz
> import Console
> import Cmd 
> import TalkIo 
> import Proto
> import ProtoIo
> import TwizCmd
> import Talk

> import Db
> import FileDb

Our point of entry. We acquire the required resources and release it at the end of use.
Since it is just Local, we dont have much to do for acquire and release, but we follow
the skeleton anyway.

> main :: IO ()
> main = getArgs >>= mymain 

From ghci prompt, use 
mymain []
to startup.

> mymain :: [String] -> IO ()
> mymain args = E.bracket (return (G stdout [] [])) return process


> process :: MyState -> IO ()
> process st = S.runStateT run st >> return ()

> quit = saveState (Fs "twiz.db") >> bye

A wee bit of initialization and greeting.

> banner =
>   do initializeTalk (Io "")
>      restoreState (Fs "Twiz.db")

> run :: Con ()
> run = banner >> forever repl

This is the place where there is some changes from the IRC version. Notably we dont
need a handle to get stuff through stdin. This is also the place we should keep a
handle to Db if we go the database route. I have not done that since it would be an overkill.

> repl =
>   do s <- recv (Prompt "> ")
>      case readProto (PIo s) of
>           Quit -> bye
>           Empty -> return ()
>           PrivMsg (u, "", msg) ->  dispatch (Io u) (getUser u) (readCmd msg)
>           Unknown s -> puts $ "unknown: " ++ show s
>           s -> puts $ "?: " ++ show s

> dispatch :: Io -> Twiz.UId -> Cmd -> Con ()
> dispatch io@(Io u) uid cmd = evalCmd u uid cmd >>= sendA io
>   where evalCmd c uid cmd
>           | isAdmin uid = adminCmd u uid cmd
>           | otherwise = userCmd u uid cmd

