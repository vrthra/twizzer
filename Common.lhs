> module Common where
> import Text.ParserCombinators.Parsec
> import Data.Char (toUpper,toLower)
> import Data.Time (Day)
> import Data.Time.Format (readTime)
> import System.Locale (defaultTimeLocale)
> import Data.Maybe
> import Control.Applicative

A few common routines.

case insensitive variants of string and char respectively.

> ichar :: Char -> CharParser st Char
> ichar c = satisfy $ \x -> toUpper c == toUpper x

> istring :: String -> CharParser st ()
> istring cs = mapM_ ichar cs <?> cs

> toSym :: String -> String
> toSym (s:ss) = toUpper s : map toLower ss

> nulldate :: Day
> nulldate = readTime defaultTimeLocale "%F" "0000-00-00"


> pT :: Parser String
> pT = many1 anyChar

> pWithSpace fn = spaces >> fn >>= (spaces >>) . return

> pWord = many1 $ noneOf ", \t\r\n"

> pDate :: Parser Day
> pDate = fmap (readTime defaultTimeLocale "%F") anid

> anid = pWithSpace pWord

> return' :: (a -> b) -> a -> Parser b
> return' = (return .)

> return'' :: (a -> b -> c) -> a -> b -> Parser c
> return'' = (return' .)

> applyE :: (a -> [b]) -> Maybe a -> [b]
> applyE = (fromMaybe [] .) . fmap

