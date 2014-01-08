> module Proto where

We do the parsing in two different stages, In the first, we analyze the protocol
and just get the information like the user or privileges from the protocol in the
second stage, we actually parse the commands. I did this in a way to model the
realworld scenario where we will have to parse the protocol like HTTP or IRC or SMTP
first, which will give us the information on username, privileges, etc. and later
the real payload. Here we define our type class which will be used to instanciate
the real parser later. We supply a parser for IRC and generic stdin input in later
files. 

> import Common

> type Name = String
> type Channel = String
> data Msg = Unknown String | Empty | PrivMsg (Name,Channel,String) | Quit | Ping
>   deriving (Show, Eq)

> class Proto a where
>   readProto :: a -> Msg

