> module ProtoIRC where

> import Text.Regex.Posix
> import Text.ParserCombinators.Parsec

> data Msg = Ping | PrivMsg (String, String, String) | Unknown String
>             deriving (Show, Eq)

> readIRC :: String -> Msg
> readIRC input = case parse ircproto "irc" input of
>     Left err -> Unknown $ "No match: " ++ (show err)
>     Right val -> val

> ircproto :: Parser Msg
> ircproto = (try parsePing) <|> (try parsePrivMsg) <|> (try parseOth)

> parseOth :: Parser Msg
> parseOth = do c <- many1 anyChar
>               return (Unknown c)

> parsePrivMsg :: Parser Msg
> parsePrivMsg = do char ':'
>                   f <- parseFrom
>                   spaces
>                   string "PRIVMSG"
>                   spaces
>                   c <- parseChannel
>                   spaces
>                   skipMany $ char ':'  -- IRC quote
>                   r <- many1 anyChar
>                   return (PrivMsg (f,c,r))

> parsePing :: Parser Msg
> parsePing = do string "PING"
>                spaces
>                h <- many1 (noneOf " ")
>                return Ping

> parseFrom :: Parser String
> parseFrom = do u <- many (noneOf " ")
>                return u

> parseChannel :: Parser String
> parseChannel = do m <- many (noneOf " ")
>                   return m

PASS db
NICK db
USER db 0 * :db
JOIN #cs583
:frege NOTICE * :Received password.
PONG : NOTICE * :Recieved user details.
:frege 001 db Welcome.
PONG : 001 db Welcome.
:frege 375 db MOTD
PONG : 375 db MOTD
:frege 372 db welcome.
PONG : 372 db welcome.
:frege 372 db :
PONG : 372 db :
:frege 376 db /MOTD.
PONG : 376 db /MOTD.
:db!db@frege.eecs.oregonstate.edu JOIN #cs583
PONG :@frege.eecs.oregonstate.edu JOIN #cs583
:frege 353 db @ #cs583 :rahul db
PONG : 353 db @ #cs583 :rahul db
:frege 366 db #cs583 :End of /NAMES list.
PONG : 366 db #cs583 :End of /NAMES list.
:rahul!gopinath@localhost.localdomain PRIVMSG #cs583 !now
PRIVMSG #cs583 :Fri May 20 11:14:46 PDT 2011
:rahul!gopinath@localhost.localdomain PART :WeeChat 0.3.2
PONG :!gopinath@localhost.localdomain PART :WeeChat 0.3.2
:rahul!gopinath@localhost.localdomain JOIN #cs583
PONG :!gopinath@localhost.localdomain JOIN #cs583
