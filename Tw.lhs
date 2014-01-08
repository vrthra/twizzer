> module Tw where
> import Text.Regex.Posix

> import Text.ParserCombinators.Parsec


The Twizzer system contains two different levels:

(1) An administrative level for the creation of twizzes, the definition of
   deadlines for twizzes and reviews, transitioning between twizzer phases
   (tasks vs. reviews), managing users and assigning buddies for reviews,
   tabulating results, etc. and

(2) a user level that allows users to submit solutions, see solutions of
   other users for which they have to prepare a review, submit reviews, view
   reviews of their own submissions, etc.

The twizzer system can be understood as a state transition system. Each twiz
goes through different phases, and each phase gives users specific access to
the system. The transition between the phases should be implemented through the
invocation of a function from the administrator. Here is a summary of the
different phases.

(1) Setup. In this initial phase the twiz is defined by the administrator,
   together with deadlines for twiz and review submissions and assignment of
   buddies.

(2) Task. During this phase, users can submit solutions. They cannot see other
   users’ solutions.

(3) Review. In the review phase users can see the solutions of those other users
   whom they have been assigned to as review buddies, and they can submit
   reviews for those solutions. Users cannot see the solution or reviews of other
   users, and they cannot see the review for their own solution during this phase.

(4) Closing. In this phase, all users can see the reviews created by their
   buddies. As an optional feature, the administrator may be able to make public
   some solutions and reviews (to provide good and not so good examples).

This specification still leaves open many details. In particular, the review
assignments (buddies), the reviews, and the published examples (of solutions and
reviews) could all be kept anonymous, or the authors of reviews could be revealed
to support accountability.

> type Id = Int
> type TId = String
> type Pwd = String

> data Review = Buddy Id (Maybe String)
>             deriving (Show, Eq)

> data User = User {id :: Id, ans :: (Maybe String), myreview :: [Review]}
>             deriving (Show, Eq)
> type Users = [User]

> type BuddyMap = [(Id, Id)]

> data Twiz = Twiz {tid :: TId, content:: String, submit:: Date, review::Date, users::Users}
>             deriving (Show, Eq)

> data Status = Setup | Task | Review | Closing | Closed
>             deriving (Show, Eq, Read)
> type Db = [(Twiz,Status)]

> data Date = D Int Int Int
>             deriving (Show, Eq)
> nulldate = D 0 0 0


-> setup :: TId -> String -> Date -> BuddyMap -> Twiz
-> setup id content date map = Twiz id content date map

-> users = [User 1 "one", User 2 "two", User 3 "three" ]

-> newtwiz = setup "1" "Hello There!!" (D 0 0 9) [(1,2),(2,3),(3,1)]

> getBuddies::Id->BuddyMap->[Id]
> getBuddies id buddyMap = map snd $ filter (\(i,j) -> i == id) buddyMap  


> data TAction = TCreate TId String |
>                TDeadline TId Date Date |
>                TBuddy TId BuddyMap |
>                TStatus TId Status |
>                TShowSoln TId Id |
>                TShow TId |
>                TUsers [(String, Int)] |
>                TListUsers |
>                TSubmit TId String |
>                TShowReview TId |
>                TSubmitReview TId Id String |
>                TList |
>                THelp |
>                TUnknown String
>               deriving (Show, Eq)
> readAction :: String -> TAction
> readAction input = case parse action "twiz" input of
>   Left err -> TUnknown $ show err
>   Right val -> val

> action :: Parser TAction
> action = (try parseCreate) <|>
>          (try parseDeadline) <|>
>          (try parseBuddy) <|>
>          (try parseUsers) <|>
>          (try parseStatus) <|>
>          (try parseShow) <|>
>          (try parseListUsers) <|>
>          (try parseList) <|>
>          (try parseShowSoln) <|>
>          (try parseSubmit) <|>
>          (try parseSubmitReview) <|>
>          (try parseHelp) <|>
>          (try parseUnknown)

> parseCreate :: Parser TAction
> parseCreate = do string "create"
>                  spaces
>--                  id <- many1 digit
>                  id <- many1 letter
>                  spaces
>                  content <- many1 anyChar
>--                  return (TCreate (read id) content)
>                  return (TCreate id content)

> parseStatus :: Parser TAction
> parseStatus = do string "set"
>                  spaces
>                  id <- many1 letter
>                  spaces
>                  string "to"
>                  spaces
>                  st <- (string "Setup") <|>
>                        (string "Task") <|>
>                        (string "Review") <|>
>                        (string "Closing") <|>
>                        (string "Done")
>                  return $ TStatus id (read st ::Status)

-> status = do {try string "Task"} <|>
->          do {try string "Review"} <|>
->          do {try string "Closing"} <|>
->          do {try string "Closed"}

> parseDeadline :: Parser TAction
> parseDeadline = do string "submit"
>                    spaces
>                    id <- many1 letter
>                    spaces
>                    string "by"
>                    spaces
>                    submit <- parseDate
>                    spaces
>                    string "review"
>                    spaces
>                    review <- parseDate
>                    return $ TDeadline id submit review 

> parseDate = do d <- count 2 digit
>                char '.'
>                m <- count 2 digit
>                char '.'
>                y <- count 2 digit
>                return $ D (read d) (read m) (read y)

> parseShow :: Parser TAction
> parseShow = do string "show"
>                spaces
>                id <- many1 letter
>--                id <- many1 digit
>--                return $ TShow (read id)
>                return $ TShow id

> parseShowSoln :: Parser TAction
> parseShowSoln = do string "get"
>                    spaces
>                    sid <- many1 letter
>                    spaces
>                    string "by"
>                    spaces
>                    id <- many1 digit
>                    return $ TShowSoln sid (read id)


> parseSubmit :: Parser TAction
> parseSubmit = do string "answer"
>                  spaces
>                  id <- many1 letter
>                  spaces
>                  content <- many1 anyChar
>                  return $ TSubmit id content

> parseSubmitReview :: Parser TAction
> parseSubmitReview = do string "review"
>                        spaces
>                        tid <- many1 letter
>                        spaces
>                        string "for"
>                        spaces
>                        id <- many1 digit
>                        content <- many1 anyChar
>                        spaces
>                        return $ TSubmitReview tid (read id) content


> parseList :: Parser TAction
> parseList = do string "list"
>                return TList


> parseHelp :: Parser TAction
> parseHelp = do string "help"
>                return THelp

> parseBuddy :: Parser TAction
> parseBuddy = do string "assign"
>                 spaces
>                 id <- many1 letter
>                 spaces
>                 lst <- sepBy (many1 idpair) (char ' ')
>                 return $ TBuddy id $ concat lst 

> parseUsers :: Parser TAction
> parseUsers = do string "users"
>                 spaces
>                 lst <- sepBy (many1 userid) (char ' ')
>                 spaces
>                 return $ TUsers $ concat lst 

> parseShowReview :: Parser TAction
> parseShowReview = do string "showreview"
>                      spaces
>                      tid <- many1 letter
>                      spaces
>                      return $ TShowReview tid


> parseListUsers :: Parser TAction
> parseListUsers = do string "userlist"
>                     return TListUsers


> userid = do p1 <- many1 letter
>             char '='
>             p2 <- many1 digit
>             return (p1, (read p2)::Id)



> idpair = do p1 <- many1 digit
>             char '='
>             p2 <- many1 digit
>             return ((read p1)::Id, (read p2)::Id)


> parseUnknown = many1 anyChar >>= return . TUnknown

-> onUserHelp (f,c,str) = do privmsg c $ "Available Commands:"
->                           privmsg c $ " list                  -- list all twizes         (*)"
->                           privmsg c $ " show <tid>            -- show the twiz           (*)"
->                           privmsg c $ " answer <tid> <content>  -- submit my answer      (task)"
->                           privmsg c $ " get <tid> by <id> -- show solution by id         (review)"
->                           privmsg c $ " review <tid> for <id> <content> -- submit review (review)"
->                           privmsg c $ " showreview <tid> -- show my reviews              (closing)"

-> onAdminHelp (f,c,str) = do onUserHelp (f,c,str)
->                            privmsg c $ " users user=id ...                               ()"
->                            privmsg c $ " userlist                                        ()"
->                            privmsg c $ " create <tid> <content>                          ()"
->                            privmsg c $ " submit <tid> by <date> review <date>            (setup)"
->                            privmsg c $ " assign <tid> b1=b2  b2=b3 ...                   (setup)"
->                            privmsg c $ " set <tid> to <Task|Review|Closing|Done>         (*)"

