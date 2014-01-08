> module Utils where
> import Text.ParserCombinators.Parsec
> import Data.Char (toUpper,toLower,chr,ord)

case insensitive variants of string and char respectively.

> ichar :: Char -> CharParser st Char
> ichar c = satisfy $ \x -> toUpper c == toUpper x

> istring :: String -> CharParser st ()
> istring cs = mapM_ ichar cs <?> cs

> toSym :: String -> String
> toSym (s:ss) = toUpper s : map toLower ss

