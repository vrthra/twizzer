> module TalkIo where
> import Console
> import Talk

The STDIN/OUT interface to twiz. We just need to print a prompt string and get
the response back.

> data Io = Io String | Prompt String

> instance Talk Io where
>   send (Io c) = puts
>   sendA c = foldr ((>>) . send c) (return ())
>   initializeTalk (Io c) = puts "Welcome to Twiz."
>   recv (Prompt c) = put c >> gets

> getUser :: String -> String
> getUser = id

