> module TalkIRC where
> import qualified Control.Monad.State as S
> import Text.Printf
> import System.IO
> import Console
> import Talk
> import Twiz

Our IRC interface. We dont need a lot of things to support IRC, Just the ability
to write to the network and info of which user communicated with us.
Unfortunately uploading is not simple with IRC, I can do that with CTCP however,
that is going much beyond the scope of this project.

server = "frege.eecs.oregonstate.edu"

> port   = 6667
> chan   = "#cs583"
> nick   = "db"
> pass   = "db"

> data IRC = IRC String Handle

> instance Talk IRC where
>   send (IRC c h) = privmsg h c
>   sendA (IRC c h) = foldr ((>>) . privmsg h c) (return ())
>   initializeTalk (IRC c h) =
>       do write h "PASS" pass
>          write h "NICK" nick
>          write h "USER" (nick ++ " 0 * :db")
>          write h "JOIN" c
>   recv (IRC c h) = do l <- io $ hGetLine h
>                       puts l
>                       return l

The IRC user id format is kind of like the uucp format, which traces where you came from.

> getUser :: String -> String
> getUser = takeWhile (/= '!') 

> privmsg :: Handle -> String -> String -> Con ()
> privmsg h c s = write h "PRIVMSG" (c ++ " :" ++ s)

> write :: Handle -> String -> String -> Con ()
> write h s t =
>   do io $ hPrintf h "%s %s\r\n" s t
>      io $ printf "> %s %s\n" s t

> pong h x    = write h "PONG" (':' : drop 6 x)

If the channel did not contain a '#' in the beginning, then it is a private message, and
in that case channel would be our own name. If so, use the requestor's id (u) as the 
channel to communicate back.

> getChan u c = case c of
>   ('#':_) -> c
>   _ -> u

