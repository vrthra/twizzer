> {-# LANGUAGE TupleSections,DeriveDataTypeable #-}

> module Main where
> import System.IO
> import System (getArgs)
> import Network
> import Text.Printf
> import Control.Monad (forever)
> import qualified Control.Monad.State as S
> import qualified Control.Exception as E

> import Console
> import Cmd 
> import Twiz
> import TwizCmd
> import Proto
> import ProtoIRC
> import Talk
> import TalkIRC 

> import Db 
> import FileDb 

Our point of entry. We acquire the required resources (FD, DB connection, network..)
And release it at the end of use. This is the IRC point of entry.

> main :: IO ()
> main = getArgs >>= mymain 

From ghci prompt, use
mymain ["myhost"] 
to startup.

> mymain :: [String] -> IO ()
> mymain args = E.bracket (acquire args) (hClose . socket) process
>   where acquire a = notify a $ do h <- connectTo (head a) $ PortNumber $ fromIntegral port
>                                   hSetBuffering h NoBuffering
>                                   return $ G h [] []
>         notify args = E.bracket_ (printf "Connecting to %s ... " (head args) >> hFlush stdout)
>                                  (putStrLn "done.")


We wrap the stuff in a stateT monad, So now we have a place to keep our global state.

> process :: MyState -> IO ()
> process st = S.runStateT run st >> return ()

> quit = saveState (Fs "Twiz.db") >> bye

A wee bit of initialization and greeting.

> banner =
>   do g <- S.get
>      initializeTalk (IRC chan (socket g))
>      restoreState (Fs "Twiz.db")

> run :: Con ()
> run = banner >> forever repl

We detect if the user has typed in a command by checking for ! here. Thus we have three levels of 
protocols. The first is the IRC protocol, the second to detect if a command was addressed to us,
and third the twiz command protocol. All the three stages are required for flexibility. 

> repl =
>   do g <- S.get
>      let sock = socket g
>      s <- recv (IRC "" sock)
>      case readProto (Irc s) of
>           Ping -> pong (socket g) s
>           Empty -> return ()
>           PrivMsg (u, c, '!':msg) -> dispatch (IRC (getChan u c) sock) (getUser u) (readCmd msg)
>           PrivMsg (u, c, msg) -> puts $ "ignore: " ++ show msg
>           Unknown s -> puts $ "unknown: " ++ show s

> dispatch :: IRC -> Twiz.UId -> Cmd -> Con ()
> dispatch irc@(IRC c u) uid cmd = evalCmd c uid cmd >>= sendA irc
>   where evalCmd c uid cmd
>           | isAdmin uid = adminCmd c uid cmd
>           | otherwise = userCmd c uid cmd

