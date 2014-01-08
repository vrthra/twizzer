> module ProtoIo where

We do the parsing in two different stages, In the first, we analyze the protocol
and just get the information like the user or privileges from the protocol in the
second stage, we actually parse the commands. I did this in a way to model the
realworld scenario where we will have to parse the protocol like HTTP or IRC or SMTP
first, which will give us the information on username, privileges, etc. and later
the real payload.

> import Text.ParserCombinators.Parsec
> import Data.Char (toUpper,chr,ord)
> import Common
> import Proto

> data ProtoIo = PIo String

> instance Proto ProtoIo where
>   readProto (PIo i) = case parse msgproto "twizproto" i of
>       Left err -> Unknown (show err)
>       Right val -> val

> msgproto :: Parser Msg
> msgproto = try parseQuit <|> try parsePrivMsg <|> try parseAny

> parseQuit :: Parser Msg 
> parseQuit = istring "quit" >> return Quit

> parseAny :: Parser Msg 
> parseAny =
>   do spaces
>      s <- pT
>      case (length s, head s) of
>               (0,_) -> return Empty
>               (_,'\EOT') -> return Quit -- hack
>               _ -> return $ Unknown s 

We ask the user to type user| to let us know which user we are dealing with.

> parsePrivMsg :: Parser Msg 
> parsePrivMsg =
>   do spaces
>      user <- many1 (noneOf " |")
>      spaces
>      char '|'
>      spaces
>      msg <- pT
>      return $ PrivMsg (user,"",msg)

