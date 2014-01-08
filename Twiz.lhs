>{-# LANGUAGE TupleSections,StandaloneDeriving,DeriveDataTypeable,TypeSynonymInstances,FlexibleContexts #-}

> module Twiz where
> import qualified Control.Monad.State as S

> import Data.Time.Calendar
> import Data.Time.Clock

> import Prelude hiding (catch)
> import Cmd as C
> import Interact as I
> import Proto as P

> import Text.JSON as J
> import Text.JSON.Generic as JG

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
   users¿ solutions.

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

-------------------------------------------------------------------------------
General user list
-------------------------------------------------------------------------------

We have two different kinds of ids The first one is the user id. It is an
that signifies a particular user in our system. The second is a twiz id.
It is a unique Id and mnemonic for each twiz. It is also represented as a string

> type UId = String

Next we keep a list of all users in the system. (And their full name and other
details. So it is called a map)

> type UserMap = [UId]

-------------------------------------------------------------------------------
Twiz
-------------------------------------------------------------------------------

> type TId = String

We don't really need to keep a map of buddy assignments. Instead we transform it
to a data structure for each twiz that says whether a review was received or not.

> data Review = By UId (Maybe String)
>   deriving (Show, Eq, Data, Typeable)

For each twiz, we keep a list of users with their solutions and reviews on that
solution.

> data Solution = Solve {answer :: (Maybe String), reviews :: [Review]}
>   deriving (Show, Eq, Data, Typeable)
> type Ss = [(UId, Solution)]

> data Twiz = Twiz {content :: String, submit :: Day, review :: Day, solutions :: Ss}
>   deriving (Show, Eq, Data, Typeable)

> deriving instance Data Day

> data Status = Setup | Task | Review | Closing | Closed
>   deriving (Show, Eq, Read, Data, Typeable)
> type Db = [(TId, Twiz, Status)]
> tID :: (TId, Twiz, Status) -> TId
> tID (i,_,_) = i
> tTz :: (TId, Twiz, Status) -> Twiz
> tTz (_,t,_) = t
> tSt :: (TId, Twiz, Status) -> Status
> tSt (_,_,s) = s

> data MyState = G {db::Db, users::UserMap}
>   deriving (Show,Typeable,Data)


As I understand it, I require the state transformer monad to use both state and IO monad.

> type Con m = S.StateT MyState m

-------------------------------------------------------------------------------
Skeleton
-------------------------------------------------------------------------------

> repl :: (Monad m, Proto (Con m)) => Con m ()
> repl =
>   do prompt
>      s <- gets
>      case P.readProto s of
>           P.Quit -> quit
>           P.Empty -> return ()
>           P.Msg (u, msg) -> evalCmd (getUser u) (readCmd msg)
>           P.Unknown s -> puts $ "unknown: " ++ (show (s!!0))

> evalCmd :: (Monad m, Proto (Con m)) => UId -> Cmd -> Con m ()
> evalCmd uid cmd
>   | isAdmin uid = adminCmd uid cmd
>   | otherwise = userCmd uid cmd

> adminCmd :: (Monad m, Proto (Con m)) => UId -> Cmd -> Con m ()
> adminCmd u c = case c of
>   THelp -> adminHelp
>   TCreate tid content -> createTwiz tid content
>   TList -> getAllTwiz >>= putA
>   TListUsers -> getAllUsers >>= putA
>   TAddU lst -> createUsers lst
>   TUpdate tid due review -> updateReviewDate tid due review
>   TAssign tid uid lst -> assignBuddy tid uid lst
>   TUpdateTask tid next -> updateStatus tid (read next)
>   _ -> puts (show c)

> onlyOn :: (Monad m, Proto (Con m)) => [Status] -> TId -> Con m () -> Con m ()
> onlyOn = only (\t -> puts t)

> userCmd :: (Monad m, Proto (Con m)) => UId -> Cmd -> Con m ()
> userCmd u c = case c of
>   THelp -> userHelp
>   TList -> getAllTwiz >>= putA
>   TShow tid -> getTwiz tid >>= putA
>   TShowAssign tid -> onlyOn [Task] tid (meAssignedTo tid u >>= putA)
>   TSubmit tid content -> onlyOn [Task] tid (submitAnswer tid u content)
>   TSubmitReview tid uid content -> onlyOn [Review] tid (submitReview tid uid u content)
>   TShowSoln tid uid -> onlyOn [Review] tid (showAnswer tid uid u >>= puts)
>   TShowReviews tid -> onlyOn [Closing] tid (showReviews tid u >>= putA)
>   _ -> puts (show c)

> userHelp :: (Monad m, Proto (Con m)) => Con m ()
> userHelp = mapM_ puts [
>      "twiz list                              list all twizes     (*)",
>      "twiz <tid> show                        show the twiz       (*)",
>      "twiz <tid> reviewers                   show my buddies     (task)",
>      "twiz <tid> answer <content>            submit my answer    (task)",
>      "twiz <tid> get <uid> solution          get solution by uid (review)",
>      "twiz <tid> for <uid> review <content>  submit review       (review)",
>      "twiz <tid> reviews                     show reviews for me (closing)",
>      "*twiz <tid> upload <file>              submit my answer    (task)",
>      "*twiz <tid> for <uid> upload <file>    submit my review    (review)"]

> adminHelp :: (Monad m, Proto (Con m)) => Con m ()
> adminHelp = mapM_ puts [
>      "*users u1,u2,u3..                                   ()",
>      "*allusers                                           ()",
>      "*create <tid> <content>                             ()",
>      "*update <tid> due <date> review <date>              (setup)",
>      "*update <tid> to <task|reiew|closing|done>             (*)",
>      "*update <tid> assign <user> <to> buddy1,buddy2,..      (setup)"]


> save :: MyState -> IO ()
> save = (writeFile "Twiz.db"). J.encode . JG.toJSON

> restore :: IO MyState
> restore = readFile "Twiz.db" >>= return . strip . JG.fromJSON . strip . J.decode
>   where strip (Ok j) = j
>         strip (Error s) = error s

> getUser :: String -> UId
> getUser = id  -- for now.

> isAdmin :: UId -> Bool
> isAdmin = (== "admin")

> only err status tid fn =
>   do t <- S.get
>      case (tSt (fetchTwiz tid (db t))) `elem` status of
>           True -> fn
>           False -> err $ "Twiz at " ++ (show t) ++ " operation not allowed."

-------------------------------------------------------------------------------
The real work.
-------------------------------------------------------------------------------

> updateTwiz tid fn = map $ \d@(i,t,s) -> if i == tid then (i,fn t, s) else d
> updateUser uid fn = map $ \d@(i,s) -> if i == uid then (i,fn s) else d

> updateUserTwiz tid uid fn =
>   updateTwiz tid (\(Twiz c s r soln) -> Twiz c s r (updateUser uid fn soln))

> fetchTwiz :: TId -> [(TId, Twiz, Status)] -> (TId, Twiz, Status)
> fetchTwiz tid = head . (filter ((== tid) . tID))

> fetchSoln :: UId -> [(UId, Solution)] -> (UId, Solution)
> fetchSoln uid = head . (filter ((== uid) . fst))

> createUsers :: Monad m => UserMap -> Con m ()
> createUsers users = S.modify $ \g -> G (db g) users

> createTwiz :: Monad m => TId -> String -> Con m ()
> createTwiz id content = S.modify $ \(G db u) -> G (addTwiz db u) u
>   where addTwiz [] u = [(id, newTwiz u, Setup)]
>         addTwiz (d@(i,t,s):db) u = if i == id then (i,newTwiz u,s):db else d:addTwiz db u
>         newTwiz = Twiz content nulldate nulldate . map (,nosoln)
>         nosoln = Solve Nothing []

> getAllTwiz :: Monad m => Con m [String]
> getAllTwiz =  S.get >>= return . (map showT) . db
>   where showT (tid,_,s) = "Twiz " ++ tid ++ "\t: " ++ (show s)

> getAllUsers :: Monad m => Con m [String]
> getAllUsers =  S.get >>= return . users

> getTwiz :: Monad m => TId -> Con m [String]
> getTwiz tid =S.get >>= return . showT . (fetchTwiz tid) . db
>   where showT (i,t,s) = [content t,
>                          "submission: " ++ (show (submit t)),
>                          "review: " ++ (show (review t))]

> updateStatus :: Monad m => TId -> Status -> Con m ()
> updateStatus id status = S.modify $ \(G db u) -> G (updateS id db) u
>   where updateS tid = map $ \d@(i,t,s) -> if i == tid then (i,t, status) else d

> updateReviewDate :: Monad m => TId -> Day -> Day -> Con m ()
> updateReviewDate id due review = S.modify $ \(G db u) -> G (updateTwiz id fn db) u
>   where fn (Twiz c s r sol) = Twiz c due review sol

> assignBuddy :: Monad m => TId -> UId -> [UId] -> Con m ()
> assignBuddy tid uid lst = S.modify $ \(G db u) -> G (updateUserTwiz tid uid fn db) u
>   where fn (Solve ans rev) = Solve ans [By i Nothing | i <- lst]

> submitAnswer :: Monad m => TId -> UId -> String -> Con m ()
> submitAnswer tid uid myanswer = S.modify $ \(G db u) -> G (updateUserTwiz tid uid fn db) u
>   where fn (Solve ans rev) = Solve (Just myanswer) rev

> submitReview :: Monad m => TId -> UId -> UId -> String -> Con m ()
> submitReview tid uid myuid myreview = S.modify $ \(G db u) -> G (updateUserTwiz tid uid fn db) u
>   where fn (Solve ans rev) = Solve ans (map update rev)
>         update b@(By i r) = if i == myuid then By i (Just myreview) else b

> showAnswer :: Monad m => TId -> UId -> UId -> Con m String
> showAnswer tid uid myid =
>   do blst <- meAssignedTo tid uid
>      case myid `elem` blst of
>           True -> S.get >>=
>                return . show . answer . snd . (fetchSoln uid) . solutions . tTz . (fetchTwiz tid) . db
>           False -> return $ "Sorry you("++myid++") are not a buddy of ("++uid++")"

> showReviews :: Monad m => TId -> UId -> Con m [String]
> showReviews tid myid =
>   S.get >>= return . (map show) . reviews . snd . (fetchSoln myid) . solutions . tTz . (fetchTwiz tid) . db

> getBuddyMap tid = S.get >>= return . p . solutions . tTz . (fetchTwiz tid). db
>   where p = map (\(id,s) -> (id, [i|By i _ <- reviews s]))

> meAssignedTo :: Monad m => TId -> UId -> Con m [UId]
> meAssignedTo tid uid = getBuddyMap tid >>=
>      return . (concatMap snd) . (filter (\ (id, ids) -> elem uid ids))

