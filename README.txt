Problem Statement.
----------------------------------------------------------------------------
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

----------------------------------------------------------------------------
There are two implementations in this bundle. The first one is a strictly local
implementation (Local.lhs) and the second one is the distributed implementation.

The persistance layer is implemented using filesystem, but it is implemented using
a typeclass which should allow for future database extension without pain.

