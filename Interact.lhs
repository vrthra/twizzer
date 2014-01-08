> module Interact where

> class Proto p where
>   put :: String -> p ()
>   puts :: String -> p ()
>   putA :: [String] -> p ()
>   gets :: p String
>   prompt :: p ()
>   quit :: p ()
>   banner :: p ()


