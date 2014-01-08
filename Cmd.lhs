> module Cmd where
> import Text.ParserCombinators.Parsec
> import Common
> import Data.Time
> import Data.Time.Clock.POSIX
> import Data.Time.Format
> import System.Locale

A parser for twiz commands. Note that this is different from the Protocol parsers as explained earlier.

We support the following commands for users.

twiz list                              list all twizes     (*)
twiz <tid> show                        show the twiz       (*)
twiz <tid> toreview                    show my buddies     (task)
twiz <tid> answer <content>            submit my answer    (task)
twiz <tid> get <uid> solution          get solution by uid (review)
twiz <tid> for <uid> review <content>  submit review       (review)
twiz <tid> reviews                     show reviews for me (closing)
twiz <tid> upload <file>               submit my answer    (task)
twiz <tid> for <uid> upload <file>     submit my review    (review)

And the following for admin

users u1,u2,u3..                                   ()
allusers                                           ()
save <file>                                        ()
load <file>                                        ()
create <tid> <content>                             ()
update <tid> due <date> review <date>              ()
update <tid> to <task|reiew|closing|setup>         ()
update <tid> assign <user> <to> buddy1,buddy2,..   ()
buddies <tid>                  show buddy assign   ()


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
>           | TSubmitU String String
>           | TSubmitReview String String String
>           | TSubmitReviewU String String String
>           | TShowAssign String
>           | TAddU [String]
>           | TSave String
>           | TLoad String
>           | TBuddy String
>           | TUpdate String Day Day
>           | TUpdateTask String String
>   deriving (Show, Eq)

> readCmd :: String -> Cmd
> readCmd i = case parse cmd "twizcmd" i of
>   Left err -> TUnknown (show err)
>   Right val -> val

> cmd :: Parser Cmd
> cmd = try parseCreate
>       <|> try parseHelpT
>       <|> try parseListT
>       <|> try parseListUT
>       <|> try parseAddUT
>       <|> try parseShowT
>       <|> try parseShowSolnT
>       <|> try parseShowReviewsT
>       <|> try parseSubmitT
>       <|> try parseSubmitUT
>       <|> try parseSubmitReviewT
>       <|> try parseSubmitReviewUT
>       <|> try parseUpdateT
>       <|> try parseUpTaskT
>       <|> try parseAssignT
>       <|> try parseBuddyT
>       <|> try parseShowAssignT
>       <|> try parseSaveT
>       <|> try parseLoadT
>       <|> try parseRest

> pTid :: Parser String
> pTid = anid

> pUid :: Parser String
> pUid = anid

> pFile :: Parser String
> pFile = anid

> parseRest :: Parser Cmd
> parseRest = fmap TUnknown (many anyChar)

> pTwin :: Parser a -> Parser a' -> (a -> a' -> Parser b) -> Parser b
> pTwin a a' fn = a >>= ((a' >>=) . fn)

> pTriple :: Parser a -> Parser a' -> Parser a'' -> (a -> a' -> a'' -> Parser b) -> Parser b
> pTriple a a' a'' fn = a >>= ((a' >>=) . ((a'' >>=) .) . fn)

> parseCreate :: Parser Cmd
> parseCreate = istring "create" >> pTwin pTid pT (return' . TCreate)

> wordTwiz = pWithSpace (istring "twiz")

> twizStr = (wordTwiz >>)
> twizCmd fn = wordTwiz >> pTid >>= (fn >>) . return

> parseListT :: Parser Cmd
> parseListT = twizStr $ istring "list" >> return TList

> parseHelpT :: Parser Cmd
> parseHelpT = istring "help" >> return THelp

> parseListUT :: Parser Cmd
> parseListUT = istring "allusers" >> return TListUsers

> parseShowT :: Parser Cmd
> parseShowT = fmap TShow $ twizCmd $ istring "show"

> parseShowSolnT :: Parser Cmd
> parseShowSolnT = pTwin (twizCmd (istring "get")) ((istring "solution" >>) . return =<< pUid) (return' . TShowSoln)

> parseShowReviewsT :: Parser Cmd
> parseShowReviewsT = fmap TShowReviews (twizCmd (istring "reviews"))

> parseSubmitT :: Parser Cmd
> parseSubmitT = pTwin (twizCmd (istring "answer" >> spaces)) pT (return' . TSubmit)

> parseSubmitUT :: Parser Cmd
> parseSubmitUT = pTwin (twizCmd (istring "upload" >> spaces)) pFile (return' . TSubmitU)

> parseSubmitReviewT :: Parser Cmd
> parseSubmitReviewT = pTriple (twizCmd (istring "for")) pUid (istring "review" >> pT) (return'' . TSubmitReview)

> parseSubmitReviewUT :: Parser Cmd
> parseSubmitReviewUT = pTriple (twizCmd (istring "for")) pUid (istring "upload" >> pFile) (return'' . TSubmitReviewU)

> parseUIDLst :: Parser [String]
> parseUIDLst = sepBy pUid (char ',')

> parseAddUT :: Parser Cmd
> parseAddUT = fmap TAddU $ pWithSpace (istring "users") >> parseUIDLst

> parseSaveT :: Parser Cmd
> parseSaveT = fmap TSave $ istring "save" >> pFile

> parseLoadT :: Parser Cmd
> parseLoadT = fmap TLoad $ istring "load" >> pFile

> parseBuddyT :: Parser Cmd
> parseBuddyT = fmap TBuddy $ istring "buddies" >> pTid

> parseUpdateTId :: Parser String
> parseUpdateTId = istring "update" >> pTid

> parseAssignT :: Parser Cmd
> parseAssignT = pTriple parseUpdateTId (istring "assign" >> pUid) (istring "to" >> parseUIDLst) (return'' . TAssign)

> parseShowAssignT :: Parser Cmd
> parseShowAssignT = fmap TShowAssign $ twizCmd $ istring "toreview"

> parseUpdateT :: Parser Cmd
> parseUpdateT = pTriple parseUpdateTId (istring "due" >> pDate) (istring "review" >> pDate) (return'' . TUpdate)

> parseUpTaskT :: Parser Cmd
> parseUpTaskT = pTwin parseUpdateTId (istring "to" >> spaces >> next) (return' . TUpdateTask)
>   where next = fmap toSym (string "task" <|> string "review" <|> string "closing" <|> string "setup")

