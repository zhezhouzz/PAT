val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( < ) : int -> int -> bool
val cons : int -> int list -> int list
val remove : int -> int list -> int list
val emp : int list -> bool

let[@axiom] emp_same (l1 : int list) (l2 : int list) =
  implies (emp l1 && emp l2) (l1 == l2)

let[@axiom] cons_not_eq (x : int) (l : int list) = not (cons x l == l)

let[@axiom] remove_cons1 (x : int) (l : int list) =
  implies (emp l) (remove x (cons x l) == l)

let[@axiom] remove_cons2 (x : int) (l : int list) =
  implies (emp l) (remove x (cons x (cons x l)) == cons x l)

let[@axiom] remove_emp (x : int) (l : int list) =
  implies (emp l) (remove x l == l)

(* Basic Typing *)

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]

val selectFollows : 
  < tid : int ; prevTid : int ; prevCid : int ; user : int ; follows : int list >
[@@obs]

val selectTweets : 
  < tid : int ; prevTid : int ; prevCid : int ; user : int ; tweets : int list >
[@@obs]

val updateFollows :
  < tid : int ; user : int ; follows : int list >
[@@obs]

val updateTweets :
  < tid : int ; user : int ; tweets : int list >
[@@obs]

val followReq : < user : int ; follow_o : int > [@@gen]
val followResp : < > [@@obs]
val unfollowReq : < user : int ; follow_o : int > [@@gen]
val unfollowResp : < > [@@obs]
val tweetReq : < user : int ; tweet : int > [@@gen]
val tweetResp : < > [@@obs]
val timelineReq : < user : int > [@@gen]
val timelineResp : < timeline : int list > [@@obs]


(* Read Committed *)
(* Invariant: For any transaction with tid = i, there does not exist a previous transaction with tid > i. *)
let beginT ?l:(i = (true : [%v: int])) =
  (starA (anyA - BeginT (tid >= i)), BeginT (tid == i), allA)

(* Invariant: Each transaction is committed exactly once; there does not exist a previous commit with cid > j. *)
let commit ?l:(i = (true : [%v: int])) ?l:(j = (true : [%v: int])) =
  ( (starA (anyA - Commit (tid == i || cid >= j));
     BeginT (tid == i);
     starA (anyA - Commit (tid == i || cid >= j))),
    Commit (tid == i && cid == j),
    starA (anyA - Put (tid == i) - Get (tid == i)) )

(* updates *)

let updateFollows ?l:(i = (true : [%v: int])) ?l:(u = (true : [%v: int]))
    ?l:(f = (true : [%v: int list])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateFollows (tid == i && user == u && follows == f),
    allA )

let updateTweets ?l:(i = (true : [%v: int])) ?l:(u = (true : [%v: int]))
    ?l:(t = (true : [%v: int list])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateFollows (tid == i && user == u && tweets == t),
    allA )


(* selects *)

let selectFollows =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(u = (true : [%v: int]))
      ?l:(f = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateFollows (tid == i && user == u));
         UpdateFollows (tid == pi && user == u && follows == f);
         starA (anyA - Put (tid == i && user == u));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateFollows (tid == i && user == u))),
        SelectFollows
          (tid == i && user == u && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && follows == f),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

let selectTweets =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(u = (true : [%v: int]))
      ?l:(t = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateTweets (tid == i && user == u));
         UpdateTweets (tid == pi && user == u && tweets == t);
         starA (anyA - Put (tid == i && user == u));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateTweets (tid == i && user == u))),
        SelectTweets
          (tid == i && user == u && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && tweets == t),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]


(* Cart *)

let newUserReq (i : int) ?l:(u = (true : [%v: int])) =
  ( starA (anyA - NewUserReq true - SelectFollows (user == u) - SelectTweets (user == u) - UpdateFollows (user == u) - SelectFollows (user == u)),
    NewUserReq (user == u),
    (BeginT (tid == i);
     UpdateFollows (tid == i && user == u && emp value);
     UpdateTweets (tid == i && user == u && emp value);
     Commit (tid == i);
     NewUserResp true;
     starA (anyA - NewUserReq true)) )

let newUserResp = (allA, NewUserResp true, allA)

let followReq (i : int) (l : int list) ?l:(u = (true : [%v: int]))
    ?l:(f = (true : [%v: int])) =
  ( allA,
    FollowReq (user == u && follow_o == f),
    (BeginT (tid == i);
     SelectFollows (tid == i && user == u && follows == l);
     allA;
     UpdateFollows (tid == i && user == u && follows == cons f l);
     starA (anyA - UpdateFollows (user == u));
     (* no write - write conflict *)
     Commit (tid == i);
     FollowResp true;
     allA) )

let followResp = (allA, FollowResp true, allA)

let unfollowReq (i : int) (l : int list) ?l:(u = (true : [%v: int]))
    ?l:(f = (true : [%v: int]))
  ( allA,
    UnfollowReq (user == u && unfollow_o == f),
    (BeginT (tid == i);
     SelectFollows (tid == i && user == u && follows == l);
     allA;
     UpdateFollows (tid == i && user == u && follows == remove f l);
     starA (anyA - UpdateFollows (user == u));
     Commit (tid == i);
     UnfollowResp true;
     allA) )

let unfollowResp = (allA, UnfollowResp true, allA)

let postTweetReq (i : int) (l : int list) ?l:(u = (true : [%v: int]))
    ?l:(t = (true : [%v: int])) =
  ( allA,
    PostTweetReq (user == u && tweet == t),
    (BeginT (tid == i);
     SelectTweets (tid == i && user == u && tweets == l);
     allA;
     UpdateTweets (tid == i && user == u && tweets == cons t l);
     starA (anyA - UpdateTweets (user == u));
     Commit (tid == i);
     PostTweetResp true;
     allA) )

let postTweetResp = (allA, PostTweetResp true, allA)

let timelineReq (i : int) (l : int list) ?l:(u = (true : [%v: int]))
  ( allA,
    TimelineReq (user == u),
    (BeginT (tid == i);
     SelectTweets (tid == i && user == u && tweets == l);
     TimelineResp (tweets == l);
     allA) )

let timelineResp = (allA, TimelineResp true, allA)

(* Global Properties *)
let[@goal] twitter_cc (u : int) (l : int list) =
  allA;
  UpdateFollows (user == u && follows == l);
  starA (anyA - UpdateFollows (user == u));
  SelectFollows (user == u && not (follows == l));
  allA
