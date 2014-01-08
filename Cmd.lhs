> module Cmd where

> import Text.ParserCombinators.Parsec
> import Utils
> import Data.Time
> import Data.Time.Clock.POSIX
> import Data.Time.Format
> import System.Locale


> data Cmd = TUnknown String
>           | TCreate String String
>           | TList
>           | THelp
>           | TListUsers
>           | TAssign String String [String]
>           | TShow String
>           | TShowSoln String String
>           | TShowReviews String
>           | TSubmit String String
>           | TSubmitReview String String String
>           | TShowAssign String
>           | TAddU [String]
>           | TUpdate String Day Day
>           | TUpdateTask String String
>   deriving (Show, Eq)

> nulldate = (readTime defaultTimeLocale "%F" "0000-00-00") :: Day
 
> readCmd :: String -> Cmd
> readCmd i = case parse cmd "twizcmd" i of
>   Left err -> TUnknown (show err)
>   Right val -> val

> cmd :: Parser Cmd
> cmd = (try parseCreate)
>       <|> (try parseHelpT)
>       <|> (try parseListT)
>       <|> (try parseListUT)
>       <|> (try parseAddUT)
>       <|> (try parseShowT)
>       <|> (try parseShowSolnT)
>       <|> (try parseShowReviewsT)
>       <|> (try parseSubmitT)
>       <|> (try parseSubmitReviewT)
>       <|> (try parseUpdateT)
>       <|> (try parseUpTaskT)
>       <|> (try parseAssignT)
>       <|> (try parseShowAssignT)
>       <|> (try parseRest)

> pT :: Parser String
> pT = many1 anyChar

> pTid :: Parser String
> pTid =
>   do spaces
>      t <- many1 (noneOf " ")
>      spaces
>      return t

> pUid :: Parser String
> pUid =
>   do spaces
>      t <- many1 $ letter <|> digit
>      spaces
>      return t

> pDate = 
>   do spaces
>      t <- many1 (noneOf " ") -- sorry no spaces.
>      spaces
>      return $ readTime defaultTimeLocale "%F" t

> parseRest :: Parser Cmd 
> parseRest = many anyChar >>= return . TUnknown 


> parseCreate :: Parser Cmd 
> parseCreate =
>   do istring "create"
>      tid <- pTid
>      content <- pT
>      return $ TCreate tid content

> wordTwiz = spaces >> istring "twiz" >> spaces

> twizStr fn = wordTwiz >> fn
> twizCmd fn = wordTwiz >> pTid >>= (\tid -> fn >> return tid)

> parseListT :: Parser Cmd 
> parseListT =twizStr $ istring "list" >> return TList

> parseHelpT :: Parser Cmd 
> parseHelpT = istring "help" >> return THelp

> parseListUT :: Parser Cmd 
> parseListUT = istring "allusers" >> return TListUsers

> parseShowT :: Parser Cmd 
> parseShowT = twizCmd (istring "show") >>= return . TShow

> parseShowSolnT :: Parser Cmd 
> parseShowSolnT =
>   do tid <- twizCmd (istring "get")
>      uid <- pUid
>      istring "solution"
>      return $ TShowSoln tid uid

> parseShowReviewsT :: Parser Cmd 
> parseShowReviewsT = twizCmd (istring "on") >>= return . TShowReviews 



> parseSubmitT :: Parser Cmd 
> parseSubmitT =
>   do tid <- twizCmd (istring "answer" >> spaces)
>      content <- pT
>      return $ TSubmit tid content

> parseSubmitReviewT :: Parser Cmd 
> parseSubmitReviewT =
>   do tid <- twizCmd (istring "for")
>      uid <- pUid
>      istring "review"
>      content <- pT
>      return $ TSubmitReview tid uid content


> parseAddUT :: Parser Cmd 
> parseAddUT =
>   do istring "users"
>      spaces
>      lst <- sepBy pUid (char ',')
>      return $ TAddU lst


> parseAssignT :: Parser Cmd 
> parseAssignT =
>   do istring "update"
>      tid <- pTid
>      istring "assign"
>      uid <- pUid
>      istring "to"
>      lst <- sepBy pUid (char ',')
>      return $ TAssign tid uid lst

> parseShowAssignT :: Parser Cmd 
> parseShowAssignT = twizCmd (istring "reviewers") >>= return . TShowAssign



> parseUpdateT :: Parser Cmd 
> parseUpdateT =
>   do istring "update"
>      tid <- pTid
>      istring "due"
>      due <- pDate
>      istring "review"
>      review <- pDate
>      return $ TUpdate tid due review

> parseUpTaskT :: Parser Cmd 
> parseUpTaskT =
>   do istring "update"
>      tid <- pTid
>      istring "to"
>      spaces
>      next <- (string "task")<|>(string "review")<|>(string "closing")<|>(string "done")
>      return $ TUpdateTask tid (toSym next)

