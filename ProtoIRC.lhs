> module ProtoIRC where

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

> data ProtoIRC = Irc String

> instance Proto ProtoIRC where
>   readProto (Irc i) = case parse msgproto "twizproto" i of
>       Left err -> Unknown (show err)
>       Right val -> val

> msgproto :: Parser Msg
> msgproto = try parsePrivMsg <|> try parsePing <|> try parseAny

> parsePing :: Parser Msg 
> parsePing = istring "ping" >> spaces >> pT >> return Ping

> parseAny :: Parser Msg 
> parseAny =
>   do spaces
>      s <- pT
>      return $ if null s then Empty else Unknown s

We are a little strict here. We ask the user to type exactly user| to let us know
what user we are dealing with.

> parsePrivMsg :: Parser Msg
> parsePrivMsg =
>   do char ':'
>      from <- pWord
>      pWithSpace (istring "privmsg")
>      channel <- pWord
>      spaces
>      skipMany $ char ':'  -- IRC quote
>      rest <- pT
>      return (PrivMsg (from, channel, rest))

