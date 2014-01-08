> module Proto where

We do the parsing in two different stages, In the first, we analyze the protocol
and just get the information like the user or privileges from the protocol in the
second stage, we actually parse the commands. I did this in a way to model the
realworld scenario where we will have to parse the protocol like HTTP or IRC or SMTP
first, which will give us the information on username, privileges, etc. and later
the real payload.

> import Text.ParserCombinators.Parsec
> import Data.Char (toUpper,chr,ord)
> import Utils

I use a tuple (User,String) rather than expand both into the Msg because I wish
to see Msg as just a template with a hole.

> type Name = String
> data Msg = Unknown String | Empty | Msg (Name, String) | Quit
>   deriving (Show, Eq)


> readProto :: String -> Msg
> readProto i = case parse msgproto "twizproto" i of
>   Left err -> Unknown (show err)
>   Right val -> val

> msgproto :: Parser Msg
> msgproto = (try parseQuit) <|> (try parseMsg) <|> (try parseAny)


> parseQuit :: Parser Msg 
> parseQuit =
>   do istring "quit"
>      return Quit

> parseAny :: Parser Msg 
> parseAny =
>   do spaces
>      s <- many anyChar
>      case (length s, s!!0) of
>               (0,_) -> return Empty
>               (_,'\EOT') -> return Quit -- hack
>               _ -> return $ Unknown s 


We are a little strict here. We ask the user to type exactly user| to let us know
what user we are dealing with.

> parseMsg :: Parser Msg 
> parseMsg =
>   do spaces
>      user <- many1 (noneOf " |")
>      spaces
>      char '|'
>      spaces
>      msg <- many1 anyChar
>      return $ Msg (user, msg)

