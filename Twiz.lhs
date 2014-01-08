> {-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TupleSections, TypeSynonymInstances #-}
> module Twiz where 
> import Data.Time.Calendar
> import Text.JSON.Generic (Data, Typeable)
> import Common (nulldate, applyE)
> import Data.Maybe

The core of Twiz.

-------------------------------------------------------------------------------
General user list
-------------------------------------------------------------------------------
We store our users based on their id.

> type UId = String

Next we keep a list of all users in the system. We can use this later to make sure
that all the users have assignments. (We dont - as of now.), and we can also use
this to generate an initial buddy map for each twiz.

> type UserMap = [UId]

-------------------------------------------------------------------------------
Twiz
-------------------------------------------------------------------------------

Our data structure is a giant tree. Each operation either fetches some thing
from the tree or performs a transformation on one of the nodes.

We start by specifying a short menmonic for each twiz - The twiz id. This id 
is unique for each twiz and is used to specify the affected twiz in the commands.

> type TId = String

The ReviewTxt is a a data structure that is used to hold a review, or Nothing
if a review was not available. It is attached to a data structure Review per userid.

> type ReviewTxt = Maybe String

As noted earlier, A Review is associated with each user, and holds an instance of
ReviewTxt.

> data Review = By UId ReviewTxt
>   deriving (Eq, Data, Typeable)

AnsTxt is similar to review. This is used to hold either an answer if one is available
or to indicate that no answer was updated yet.

> type AnsTxt = Maybe String

For each twiz, we keep a list of users with their solutions and reviews on that
solution. This is handled by the datastructure Solution. It contains the answer
(if there was one), along with reviews for that answer.

> data Solution = Solve {answer :: AnsTxt, reviews :: [Review]}
>   deriving (Eq, Data, Typeable)

> data USol = USol {usolId :: UId, sol::Solution}
>   deriving (Eq, Data, Typeable)

Ss is the type synonym for the tuple that associates a userid with its solution structure.

> type Ss = [(UId, Solution)]

The main data structure is Twiz which contains the content of the twiz, the submission 
deadline, review deadline, and a list of (potential) solutions. When the twiz is created,
the solutions are updated so that there is one Solution per userid that is known to the
system.

> data Twiz = Twiz {content :: String, submit :: Day, review :: Day, solutions :: Ss}
>   deriving (Eq, Data, Typeable)

This is the stand alone instance that we had to use to make sure that Day is also
serializable to JSON

> deriving instance Data Day

The status of the twiz. Note that Closed is not used as of now, however it is nice 
to have to indicate that some twizes are archived.

> data Status = Setup | Task | Review | Closing | Closed
>   deriving (Eq, Read, Data, Typeable)

The Tz structure associates a twiz with its id and the status. I decided to keep the status
separate from the twiz because it does not logically belong to the twiz but rather is related
to the administration of twiz.

> data Tz = Tz {tId::TId, tTz::Twiz, tSt::Status}
>   deriving (Eq, Data, Typeable)

And our main data base is represented by the type synonym Db. It is just a list
of Tz structures.

> type Db = [Tz]

-------------------------------------------------------------------------------
Fetch
This section contains all the getters. Absolutely no modification of the DS.
-------------------------------------------------------------------------------

fetchTwiz returns the specifed twiz from the list of twizes.

> fetchTwiz :: TId -> [Tz] -> Maybe Tz
> fetchTwiz tid = listToMaybe . filter ((== tid) . tId)

Given the uid, and list of solutions, fetchSoln is just a lookup

> fetchSoln :: UId -> [(UId, Solution)] -> Maybe Solution
> fetchSoln = lookup

fetchSolnBy  applies the fetchSol on the db. This requires the twiz id and the user id.
given twiz id and uid, it fetches the solution corresponding to that user for that twiz

> fetchSolnBy :: TId -> UId -> [Tz] -> Maybe Solution
> fetchSolnBy tid uid db = fetchTwiz tid db >>= fetchSoln uid . solutions . tTz

fetchanswer fetches the answer corresponding to the particular user for the particular twiz

> fetchAnswer :: TId -> UId -> [Tz] -> AnsTxt
> fetchAnswer tid uid db = fetchSolnBy tid uid db >>= answer

I wonder which is better? the above or this?
-> fetchAnswer tid uid = fromMaybe Nothing . fmap answer . fetchSolnBy tid uid

fetchreviews is similar to fetchanswer. The only difference is that instead of answers we 
return reviews. If either the revews were not found or the usre was not found, we return
an empty list.

> fetchReviews :: TId -> UId -> [Tz] -> [Review]
> fetchReviews tid uid = applyE reviews . fetchSolnBy tid uid

Each user is allowed to provide a solution, and a list of buddies are assigned to review
the given solution.  fetchBuddy map gives a list of users and the buddies assigned to
review him/her for a particular twiz.

> fetchBuddyMap :: TId -> [Tz] -> [(UId, [UId])]
> fetchBuddyMap tid = applyE (getids . solutions . tTz) . fetchTwiz tid
>   where getids = map $ \ (id, s) -> (id, [i|By i _ <- reviews s])

fetchIdIsReviewerOf is a sort of the reverse of fetchBuddyMap. Given a twiz, and a user id, it
returns the list of users that this user is assigned to review.

> fetchIdIsReviewerOf :: TId -> UId -> [Tz] -> [UId]
> fetchIdIsReviewerOf tid uid = map fst . filter (\ (id, ids) -> elem uid ids) . fetchBuddyMap tid

-------------------------------------------------------------------------------
Update
All the modification of our db is done here.
-------------------------------------------------------------------------------

Any function that changes the Tz data structure corresponding to a given twiz id.
To be used for changing status or updating the twiz

> onTwiz :: TId -> (Tz -> Tz) -> [Tz] -> [Tz]
> onTwiz tid fn = map $ \d@(Tz i _ _) -> if i == tid then fn d else d

updateTwiz uses the onTwiz to modify a particular twiz.

> updateTwiz :: TId -> (Twiz -> Twiz) -> [Tz] -> [Tz]
> updateTwiz tid fn = onTwiz tid $ \d@(Tz i t s) -> (Tz i (fn t) s)

updateStatus uses onTwiz to modify the status of a particular twiz.

> updateStatus :: TId -> Status -> [Tz] -> [Tz]
> updateStatus tid status = onTwiz tid $ \d@(Tz i t s) -> (Tz i t status)

updateUserDb is used to modify anything belonging to a particular user for a particular twiz. It is used
to submit answer, submit reviews for a particular user etc.

> updateUserDb :: TId -> UId -> (Solution -> Solution) -> [Tz] -> [Tz]
> updateUserDb tid uid fn = updateTwiz tid (\(Twiz c s r soln) -> (Twiz c s r (updateSol uid fn soln)))
>   where updateSol :: UId -> (Solution -> Solution) -> [(UId, Solution)] -> [(UId, Solution)]
>         updateSol uid fn = map $ \d@(i, s) -> if i == uid then (i, fn s) else d

-------------------------------------------------------------------------------
The real work. Most of it is just wrappers over our defined fetchers and updaters,
The fetched data is converted to formated strings here. The main reason for the
existance of this section is to make sure that we can at some point, localize the
messages and errors. In a sense, this also serves as our API

One decision here is to return a list of strings [String] rather than a string
that contained new lines. The justification was that new line was not exactly
platform independent, and if twiz was going to be distributed, then it was better
to explicitly indicate the individual lines.
-------------------------------------------------------------------------------

addTwiz initializes a new twiz with the given id and content. It also requires
a list of all users in the system. This one command is slightly different from
all the others in that it introduces a new twiz to the db, and hence requires
the modificaiton of db as a whole. 

> addTwiz :: [Tz] -> [UId] -> TId -> String -> [Tz]
> addTwiz [] u id content = [Tz id (newTwiz content u) Setup]
> addTwiz (d@(Tz i t s):db) u id content
>           | i == id = Tz i (newTwiz content u) s:db
>           | otherwise = d:addTwiz db u id content
> newTwiz content = Twiz content nulldate nulldate . map (, nosoln)
> nosoln = Solve Nothing []

changeStatus changes the twiz status

> changeStatus :: TId -> Status -> [Tz] -> [Tz]
> changeStatus = updateStatus

changeDates updates the review date in the twiz.

> changeDates :: TId -> Day -> Day -> [Tz] -> [Tz]
> changeDates id due review = updateTwiz id $ \(Twiz c s r sol) -> Twiz c due review sol

getAllStatus converts the twiz into an abbreviated viewable form containing just twiz id and status.

> getAllStatus :: [Tz] -> [String]
> getAllStatus =  map show

getTwiz returns the contents of the twiz as a string array.

> getTwiz :: TId -> [Tz] -> [String]
> getTwiz tid =  applyE (showA . tTz) . fetchTwiz tid

canISee checks if I have access to the solution of the given user for given twiz.

> canISee :: TId -> UId -> UId -> [Tz] -> Bool
> canISee tid uid myid db
>   | uid == myid = True
>   | otherwise = elem uid $ fetchIdIsReviewerOf tid myid db

getAnswer returns the solution of a particular twiz by a particular user. This is used by the 
reviewers of that user for that twiz. Note the error message. Note also that the user him/herself
is allowed to print his/her solution.

> getAnswer :: TId -> UId -> UId -> [Tz] -> [String]
> getAnswer tid uid myid db
>   | canISee tid uid myid db = showA $ fmtJ $ fetchAnswer tid uid db
>   | otherwise = ["Sorry you ("++myid++") are not a buddy of ("++uid++")"]

getReviews returns all the reviews corresponding to a particular twiz for my solution.

> getReviews :: TId -> UId -> [Tz] -> [String]
> getReviews tid myid = concatMap showA . fetchReviews tid myid

getBuddyMap just lists the buddy assignment for a twiz

> getBuddyMap :: TId -> [Tz] -> [String]
> getBuddyMap tid = map showBuddy . fetchBuddyMap tid

meAssignedTo lists the reviewers for my solution for a particular twiz.

> meAssignedTo :: TId -> UId -> [Tz] -> [String]
> meAssignedTo = fetchIdIsReviewerOf

Assign a reviewer for a user for a particular twiz. Note that we initialize
the reviews as NoR. When the reviewer submits the answer, it gets changed to the review txt

> assignBuddy :: TId -> UId -> [UId] -> [Tz] -> [Tz]
> assignBuddy tid uid lst = updateUserDb tid uid fn
>   where fn (Solve ans rev) = Solve ans $ map (flip By Nothing) lst

submit my answer for a particular twiz. Note that the answers can be rewritten multiple times.

> submitAnswer :: TId -> UId -> String -> [Tz] -> [Tz]
> submitAnswer tid uid myanswer = updateUserDb tid uid fn
>   where fn (Solve ans rev) = Solve (Just myanswer) rev

submit my review for the solution of a particualr user for a particular twiz

> submitReview :: TId -> UId -> UId -> String -> [Tz] -> [Tz]
> submitReview tid uid myuid myreview = updateUserDb tid uid fn
>   where fn (Solve ans rev) = Solve ans (map update rev)
>         update b@(By i r) = if i == myuid then By i (Just myreview) else b

---------------------------------------------------------------------
Formating
---------------------------------------------------------------------

> instance Show Solution where
>   show (Solve ans rev) = fmtJ ans ++ " reviews: " ++ show [i | By i _ <- rev]

> instance Show Review where
>   show (By i r) = i ++ ":\n  " ++ fmtJ r

> instance Show Status where
>   show Setup = "setup"
>   show Task = "task"
>   show Review = "review"
>   show Closing = "closing"
>   show Closed = "closed"

> instance Show Tz where
>   show (Tz i t s) = "Twiz " ++ i ++ "\t: " ++ show s

> instance Show Twiz where
>   show t = content t ++ "\n" ++ "submission: " ++ show (submit t) ++ "\n" ++ "review: " ++ show (review t)

> showA :: (Show a) => a -> [String]
> showA = lines . show

> fmtJ (Nothing) = "-"
> fmtJ (Just t) = t

> showBuddy :: (UId, [UId]) -> String
> showBuddy (a, b) = a ++ ": " ++ unwords b

