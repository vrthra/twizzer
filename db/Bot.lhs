> module Bot where
> import Data.List
> import Network
> import System.IO
> import System.Time
> import System.Exit
> import qualified Control.Monad.Reader as R
> import qualified Control.Monad.Writer as W
> import qualified Control.Monad.State as S
> import qualified Control.Exception as E
> import Text.Printf
> import Prelude hiding (catch)
> import ProtoIRC
> import Tw
> import Debug.Trace

> debug :: (Show a) => a -> a
> debug a = trace ("DEBUG: " ++ show a) a

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
   users' solutions.

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



> server = "frege.eecs.oregonstate.edu"
> port   = 6667
> chan   = "#cs583"
> nick   = "db"
> pass   = "db"

> type Net = S.StateT Bot IO
> data Bot = Bot { socket :: Handle, starttime :: ClockTime, twizdb :: Db , usrlst::[(String, Id)]}

> getTwiz:: Db -> TId -> Twiz
> getTwiz db sid = head [t | (t@(Twiz tid _ _ _ _), _) <- db, tid == sid]

> onTwiz :: (Twiz -> Twiz) -> Db -> Db
> onTwiz fn = map (\(t,s) -> (fn t,s))

> setDeadline :: TId -> Date -> Date -> Net ()
> setDeadline id s' r' = do (Bot s t db u) <- S.get
>                           ndb <- return $ onTwiz (\(Twiz sid c s r assign) -> case id == sid of
>                                                               True ->  (Twiz sid c s' r' assign)
>                                                               False ->  (Twiz sid c s r assign)) db
>                           S.put $ Bot s t ndb u

> getSoln :: Id -> TId -> Id -> Net (Maybe String)
> getSoln myid sid id = do (Bot s t db u) <- S.get
>                          usrs <- return (users (getTwiz db sid))
>                          case [(i,c)| [Buddy i c] <- [ rev | User i ans rev <- usrs, myid == i], id == i] of
>                               [(i,c)] -> return c
>                               [] -> return Nothing

> getReview :: Id -> TId -> Net [Review]
> getReview myid sid = do (Bot s t db u) <- S.get
>                         usrs <- return (users (getTwiz db sid))
>                         case [rev | User i ans rev <- usrs, myid == i] of
>                              [lst] -> return lst



> setBuddyList :: TId -> BuddyMap -> Net ()
> setBuddyList id bmap = do (Bot s t db u) <- S.get
>                           ndb <- return $ onTwiz (\(Twiz sid c s r users) -> case id == sid of
>                                                               True ->  (Twiz sid c s r (updated users))
>                                                               False ->  (Twiz sid c s r users)) db
>                           S.put $ Bot s t ndb u
>     where updated users = [ User u Nothing (buddy u bmap) | u <- map fst bmap]
>           buddy uid bmap = [ Buddy r Nothing | (u,r) <- bmap, u == uid ]

> setSubmit :: TId -> String -> Net ()
> setSubmit id soln = do (Bot s t db u) <- S.get
>                        ndb <- return $ onTwiz (\(Twiz sid c s r assign) -> case id == sid of
>                                                              True ->  (Twiz sid soln s r assign)
>                                                              False -> (Twiz sid c s r assign)) db
>                        S.put $ Bot s t ndb u

> setReview :: TId -> (Id, String) -> Net ()
> setReview rid (id, str) = do (Bot s t db u) <- S.get
>                              ndb <- return $ onTwiz (\(Twiz sid c s r users) -> case rid == sid of
>                                                               True ->  (Twiz sid c s r (updated users id str))
>                                                               False ->  (Twiz sid c s r users)) db
>                              S.put $ Bot s t ndb u
>     where updated users id str = [ User id ans (update myrev id str) | User id ans myrev <- users]
>           update myrev id str = map (\b@(Buddy bid bstr) -> case bid == id of
>                                           True -> Buddy bid (Just str)
>                                           False -> b ) myrev


> setStatus :: TId -> Status -> Net ()
> setStatus id status = do (Bot s t db u) <- S.get
>                          ndb <- return $ map (\(t@(Twiz sid _ _ _ _),s) -> case id == sid of
>                                                               True ->  (t,status)
>                                                               False ->  (t,s)) db
>                          S.put $ Bot s t ndb u

> createTwiz :: TId -> String -> Net ()
> createTwiz id content = do  (Bot s t db u) <- S.get
>                             S.put $ Bot s t ((Twiz id content nulldate nulldate [], Setup):db) u

> showTwiz :: TId -> Net Twiz
> showTwiz id = do (Bot s t db u) <- S.get
>                  return $ getTwiz db id
> listTwiz :: Net [(TId, Status)]
> listTwiz = do (Bot s t db u) <- S.get
>               return $ [ (tid,s) | (Twiz tid _ _ _ _, s) <- db]

> botmain :: IO ()
> botmain = E.bracket connect disconnect process
>   where disconnect = (hClose . socket)
>         connect = notify $ do t <- getClockTime
>                               h <- connectTo server $ PortNumber $ fromIntegral port
>                               hSetBuffering h NoBuffering
>                               return (Bot h t ([]::Db) [])
>         notify a = E.bracket_ (printf "Connecting to %s ... " server >> hFlush stdout)
>                               (putStrLn "done.")
>                               a

> process :: Bot -> IO ()
> process st = do S.runStateT run st
>                 return ()

> run :: Net ()
> run = do write "PASS" pass
>          write "NICK" nick
>          write "USER" (nick++" 0 * :db")
>          write "JOIN" chan
>          b <- S.get 
>          listen (socket b)
 
> listen :: Handle -> Net ()
> listen h = forever (loop h)
>   where forever a = a >> forever a

> loop h = do s <- init `fmap` io (hGetLine h)
>             io $ putStrLn ("| " ++ s)
>             case (readIRC s) of
>                  Ping -> pong s
>                  msg -> eval msg
> pong x    = write "PONG" (':' : drop 6 x)
> privmsg :: String -> String -> Net ()
> privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)
 
> write :: String -> String -> Net ()
> write s t = do
>     b <- S.get
>     io $ hPrintf (socket b) "%s %s\r\n" s t
>     io $ printf    "> %s %s\n" s t
 
> io :: IO a -> Net a
> io = S.liftIO

> getNick :: String -> String
> getNick = takeWhile (/= '!') 
> eval :: Msg -> Net ()
> eval (PrivMsg (_,_,"!quit"))  = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
> eval (PrivMsg (f,c,str)) = case str of
>           ('!':_) -> case c of
>                           ('#':_) -> twizAction (nick, c, str)
>                           _ -> twizAction (nick, nick, str)
>           _ ->  do io $ putStrLn ("1: " ++ str)
>                    return ()
>     where nick = getNick f
> eval (Unknown str) = do io $ putStrLn ("u: " ++ str)
>                         return ()

> twizAction :: (String, String, String) -> Net ()
> twizAction (f, c, str) = case f of
>                               "admin" -> actAdmin (f, c, drop 1 str)
>                               _ -> actUser (f, c, drop 1 str)

> actAdmin :: (String, String, String) -> Net ()
> actAdmin (f, c, str) = case readAction str of
>                (THelp) -> onAdminHelp (f,c,str)
>                (TCreate id content) -> do createTwiz id content
>                                           privmsg c $ "created " ++ id
>                (TDeadline id submit review) -> setDeadline id submit review
>                (TBuddy id assign) -> setBuddyList id assign
>                (TList) -> onList (f,c,str)
>                (TListUsers) -> onListUid (f,c,str)
>                (TUsers lst) -> onUsers (f,c,str) lst
>                (TStatus id s) -> do setStatus id s
>                                     privmsg chan $ " twiz " ++ id ++ "changed to " ++ (show s)
>                (TShow id) -> do twiz <- showTwiz id
>                                 privmsg c $ " " ++ id ++ ": " ++ (content twiz)
>                x -> privmsg c $ (show x)

> onList (f,c,str)= do lst <- listTwiz
>                      loop lst
>                      return ()
>                      where loop [] = return ()
>                            loop ((x,y):xs) = do loop xs
>                                                 privmsg c $ x ++ ": " ++ (show y)

> onUsers (f,c,str) lst = do (Bot s t db u) <- S.get
>                            S.put $ Bot s t db lst

> onListUid (f,c,str) =  do (Bot s t db u) <- S.get
>                           showu u --privmsg c $ show (map (\(x,y) -> x ++ ":" ++ (show y)) u)
>    where showu [] = return ()
>          showu ((i,j):xs) = do privmsg c $ " " ++ (show j) ++ ":" ++ i
>                                showu xs

> onShow (f,c,str) id = do twiz <- showTwiz id
>                          privmsg c $ " " ++ id ++ ": " ++ (content twiz)

> onUserHelp (f,c,str) = do privmsg c $ "Available Commands:"
>                           privmsg c $ " list                  -- list all twizes         (*)"
>                           privmsg c $ " show <tid>            -- show the twiz           (*)"
>                           privmsg c $ " answer <tid> <content>  -- submit my answer      (task)"
>                           privmsg c $ " get <tid> by <id> -- show solution by id         (review)"
>                           privmsg c $ " review <tid> for <id> <content> -- submit review (review)"
>                           privmsg c $ " showreview <tid> -- show my reviews              (closing)"

> onAdminHelp (f,c,str) = do onUserHelp (f,c,str)
>                            privmsg c $ " users user=id ...                               ()"
>                            privmsg c $ " userlist                                        ()"
>                            privmsg c $ " create <tid> <content>                          ()"
>                            privmsg c $ " submit <tid> by <date> review <date>            (setup)"
>                            privmsg c $ " assign <tid> b1=b2  b2=b3 ...                   (setup)"
>                            privmsg c $ " set <tid> to <Task|Review|Closing|Done>         (*)"


> onShowSoln (f,c,str) sid id = do s <- getStatus sid
>                                  uid <- getUid f
>                                  case s of
>                                       [Review] -> do soln <- getSoln uid sid id
>                                                      case soln of
>                                                           Nothing -> privmsg c $ "Buddy was not found."
>                                                           Just s -> privmsg c $ s
>                                       [s] -> privmsg c $ "At status " ++ (show s)
>                                       [] -> privmsg c $ "Not found"
                                  

> onShowReview (f,c,str) sid = do s <- getStatus sid
>                                 uid <- getUid f
>                                 case s of
>                                       [Closing] -> do review <- getReview uid sid
>                                                       lstReviews review c
>                                       [s] -> privmsg c $ "At status " ++ (show s)
>                                       [] -> privmsg c $ "Not found"
                                  


> onSubmit (f,c,str) id content = do s <- getStatus id
>                                    case s of
>                                       [Task] -> do setSubmit id content
>                                                    privmsg c $ "done."
>                                       [s] -> privmsg c $ "At status " ++ (show s)
>                                       [] -> privmsg c $ "Not found"

> onReview (f,c,str) tid id content = do s <- getStatus tid
>                                        case s of
>                                             [Review] -> do setReview tid (id,content)
>                                                            privmsg c $ "done."
>                                             [s] -> privmsg c $ "At status " ++ (show s)
>                                             [] -> privmsg c $ "Not found"

> lstReviews [] c = return ()
> lstReviews (x:xs) c = do privmsg c $ (show x)
>                          lstReviews xs c


> getStatus id = do ts <- listTwiz
>                   return [s | (i,s) <- ts, id == i]

> getUid f = do (Bot s t db u) <- S.get
>               case lookup f u of
>                   Just b -> return b
>                   Nothing -> error "not found."

> actUser :: (String, String, String) -> Net ()
> actUser (f, c, str) =  case readAction str of
>                (THelp) -> onUserHelp (f,c,str)
>                (TList) -> onList (f,c,str)
>                (TShow id) -> onShow (f,c,str) id
>                (TShowSoln tid id) -> onShowSoln (f,c,str) tid id
>                (TSubmit id soln) -> onSubmit (f,c,str) id soln
>                (TSubmitReview tid id soln) -> onReview (f,c,str) tid id soln
>                (TShowReview tid ) -> onShowReview (f,c,str) tid
>                x -> privmsg c $ ("Not understood (user):" ++ str)

