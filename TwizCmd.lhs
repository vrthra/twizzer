> {-# LANGUAGE TupleSections #-}
> module TwizCmd where
> import qualified Control.Monad.State as S

> import Twiz 
> import Console 
> import Cmd 
> import FileDb 

> import qualified Db as D
> import qualified FileDb as FD

Some of the commands (the onDb ones) modify the D.S while the others
Just fetch a list of strings (toDb). So I cannot avoid the use of state
here.

> isAdmin :: UId -> Bool
> isAdmin = (== "admin")

> adminCmd :: String -> UId -> Cmd -> Con [String]
> adminCmd chan u c = case c of
>   THelp -> adminHelp
>   TCreate tid content -> onDbU $ \db u -> addTwiz db u tid content
>   TList -> toDb getAllStatus
>   TListUsers -> toUsers
>   TAddU lst -> updateUsers (const lst) >> return []
>   TBuddy tid -> toDb (getBuddyMap tid)
>   TUpdate tid due review -> onDb $ changeDates tid due review
>   TAssign tid uid lst -> onDb (assignBuddy tid uid lst)
>   TUpdateTask tid next -> onDb $ changeStatus tid $ read next
>   TSave file -> D.saveState (Fs file) >> return []
>   TLoad file -> D.restoreState (Fs file) >> return []
>   TUnknown c -> return ["ignored:",show c]
>   c -> puts (show c) >> return []

> userCmd :: String -> UId -> Cmd -> Con [String]
> userCmd chan u c = case c of
>   THelp -> userHelp
>   TList -> toDb getAllStatus
>   TShow tid -> toDb (getTwiz tid)
>   TShowAssign tid ->
>       only [Task] tid $ toDb $ meAssignedTo tid u
>   TSubmit tid content ->
>       only [Task] tid $ onDb $ submitAnswer tid u content
>   TSubmitU tid file ->
>       only [Task] tid $ readF file $ onDb . submitAnswer tid u
>   TSubmitReviewU tid uid file ->
>       only [Review] tid $ readF file $ onDb . submitReview tid uid u
>   TSubmitReview tid uid content ->
>       only [Review] tid $ onDb $ submitReview tid uid u content
>   TShowSoln tid uid ->
>       only [Review] tid $ toDb $ getAnswer tid uid u
>   TShowReviews tid ->
>       only [Closing] tid $ toDb $ getReviews tid u
>   TUnknown c -> puts ("\tXign:" ++ show c) >> return []
>   c -> puts (show c) >> return []

> userHelp :: Con [String]
> userHelp = return [
>      "twiz list                              list all twizes     (*)",
>      "twiz <tid> show                        show the twiz       (*)",
>      "twiz <tid> toreview                    show my buddies     (task)",
>      "twiz <tid> answer <content>            submit my answer    (task)",
>      "twiz <tid> get <uid> solution          get solution by uid (review)",
>      "twiz <tid> for <uid> review <content>  submit review       (review)",
>      "twiz <tid> reviews                     show reviews for me (closing)",
>      "twiz <tid> upload <file>               submit my answer    (task)",
>      "twiz <tid> for <uid> upload <file>     submit my review    (review)"]

> adminHelp :: Con [String]
> adminHelp = return [
>      "*users u1,u2,u3..                                   ()",
>      "*allusers                                           ()",
>      "*save <file>                                        ()",
>      "*load <file>                                        ()",
>      "*create <tid> <content>                             ()",
>      "*update <tid> due <date> review <date>              ()",
>      "*update <tid> to <task|reiew|closing|setup>         ()",
>      "*update <tid> assign <user> <to> buddy1,buddy2,..   ()",
>      "*buddies <tid>                  show buddy assign   ()"]

> only status tid fn =
>   do t <- S.get
>      case s t of
>         Nothing -> return ["Twiz does not exist."]
>         Just x -> if tSt x `elem` status then fn
>                     else return ["Twiz at " ++ show (s t) ++ " operation not allowed."]
>   where s = fetchTwiz tid . db

---------------------------------------------------------------------

> readF :: String -> (String -> Con b) -> Con b
> readF = (>>=) . io . readFile

> toDb :: (Db -> [String]) -> Con [String]
> toDb fn = fmap (fn . db) S.get

> toUsers :: Con UserMap
> toUsers = fmap users S.get

> updateDb :: (Db -> Db) -> Con ()
> updateDb fn = S.modify $ \(G h db u) -> G h (fn db) u

> onDb :: (Db -> Db) -> Con [String]
> onDb fn = updateDb fn >> return []

> updateUsers :: (UserMap -> UserMap) -> Con ()
> updateUsers fn = S.modify $ \(G h db u) -> (G h db (fn u))

> onDbU :: (Db -> UserMap -> Db) -> Con [String]
> onDbU fn = S.modify (\(G h db u) -> (G h (fn db u) u)) >> return []

