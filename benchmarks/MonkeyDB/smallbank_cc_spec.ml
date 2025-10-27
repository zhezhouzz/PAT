val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( < ) : int -> int -> bool
val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
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

val selectAccounts : 
  < tid : int ; prevTid : int ; prevCid : int ; name : int ; custid : int >
[@@obs]

val selectSavings : 
  < tid : int ; prevTid : int ; prevCid : int ; custid : int ; balance : int >
[@@obs]

val selectChecking : 
  < tid : int ; prevTid : int ; prevCid : int ; custid : int ; balance : int >
[@@obs]


val updateAccounts :
  < tid : int ; name : int ; custid : int >
[@@obs]

val updateSavings :
  < tid : int ; custid : int ; balance : int >
[@@obs]

val updateChecking :
  < tid : int ; custid : int ; balance : int >
[@@obs]


val openAccountsReq : < name : int ; custid : int > [@@gen]
val openAccountsResp : < > [@@obs]
val amalgamateReq : < custid0 : int ; custid1 : int > [@@gen]
val amalgamateResp : < > [@@obs]
val balanceReq : < name : int > [@@gen]
val balanceResp : < balance : int > [@@obs]
val depositCheckingReq : < name : int ; amount : int > [@@gen]
val depositCheckingResp : < > [@@obs]
val sendPaymentReq : < srcid : int ; destid : int ; amount : int > [@@gen]
val sendPaymentResp : < > [@@obs]
val transactSavingsReq : < name : int ; amount : int > [@@gen]
val transactSavingsResp : < > [@@obs]
val writeCheckReq : < name : int ; amount : int > [@@gen]
val writeCheckResp : < > [@@obs]


(* Causal Consistency *)
(* Invariant: For any transaction with tid = i, there does not exist a previous transaction with tid > i. *)
let beginT ?l:(i = (true : [%v: int])) =
  (starA (anyA - BeginT (tid >= i)), BeginT (tid == i), allA)

(* Invariant: Each transaction is committed exactly once; there does not exist a previous commit with cid > j. *)
let commit ?l:(i = (true : [%v: int])) ?l:(j = (true : [%v: int])) =
  ( (starA (anyA - Commit (tid == i || cid >= j));
     BeginT (tid == i);
     starA (anyA - Commit (tid == i || cid >= j))),
    Commit (tid == i && cid == j),
    starA (anyA - UpdateAccounts (tid == i) - SelectAccounts (tid == i)
                - UpdateSavings (tid == i) - SelectSavings (tid == i)
                - UpdateChecking (tid == i) - SelectChecking (tid == i)) )

(* updates *)

let updateAccounts ?l:(i = (true : [%v: int])) ?l:(c = (true : [%v: int]))
    ?l:(n = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateAccounts (tid == i && custid == c && name == n),
    allA )

let updateSavings ?l:(i = (true : [%v: int])) ?l:(c = (true : [%v: int]))
    ?l:(b = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateSavings (tid == i && custid == c && balance == b),
    allA )

let updateChecking ?l:(i = (true : [%v: int])) ?l:(c = (true : [%v: int]))
    ?l:(b = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateChecking (tid == i && custid == c&& balance == b),
    allA )


(* selects *)

let selectAccounts =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(c = (true : [%v: int]))
      ?l:(n = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateAccounts (tid == i && custid == c));
         UpdateAccounts (tid == pi && custid == c && name == n);
         starA (anyA - UpdateAccounts (tid == i && custid == c));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateAccounts (tid == i && custid == c))),
        SelectAccounts
          (tid == i && custid == c && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && name == n),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

let selectSavings =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(c = (true : [%v: int]))
      ?l:(b = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateSavings (tid == i && custid == c));
         UpdateSavings (tid == pi && custid == c && balance == b);
         starA (anyA - UpdateSavings (tid == i && custid == c));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateSavings (tid == i && custid == c))),
        SelectSavings
          (tid == i && custid == c && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && balance == b),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

let selectChecking =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(c = (true : [%v: int]))
      ?l:(b = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateChecking (tid == i && custid == c));
         UpdateChecking (tid == pi && custid == c && balance == b);
         starA (anyA - UpdateChecking (tid == i && custid == c));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateChecking (tid == i && custid == c))),
        SelectChecking
          (tid == i && custid == c && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && balance == b),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]


(* SmallBank *)
(* TODO: *allowing operations between selects and updates according to causal consistency guidelines
         *fleshing out history and future for operations
         *adding information about balances to these checks
         *adding a goal
         *adding custname to api for some operations *)

let openAccountsReq (i : int) ?l:(n = (true : [%v: int])) ?l:(c = (true : [%v: int])) =
  ( starA (anyA - OpenAccountsReq(name == n) - OpenAccountsReq(custid == c)),
    OpenAccountsReq (name == n && custid == c),
    (BeginT (tid == i);
     UpdateAccounts (tid == i && name == n && custid == c);
     UpdateSavings (tid == i && custid == c && balance == 0);
     UpdateChecking (tid == i && custid == c && balance == 0);
     Commit (tid == i);
     OpenAccountsResp true;
     allA ) )

let openAccountsResp = (allA, OpenAccountsResp true, allA)

let amalgamateReq (i : int) ?l:(c0 = (true : [%v: int])) ?l:(c1 = (true : [%v: int])) =
  ( (allA;
     OpenAccountsReq (custid == c0 || custid == c1);
     allA;
     OpenAccountsReq (custid == c0 || custid == c1);
     allA),
    AmalgamateReq (custid0 == c0 && custid1 == c1),
    (BeginT (tid == i);
     SelectSavings (tid == i && custid == c0);
     SelectChecking (tid == i && custid == c0);
     SelectChecking (tid == i && custid == c1);
     allA;
     UpdateSavings (tid == i && custid == c0 && balance == 0);
     UpdateChecking (tid == i && custid == c0 && balance == 0);
     UpdateChecking (tid == i && custid == c1);
     starA (((anyA - UpdateSavings (custid == c0)) - UpdateChecking (custid == c0)) - UpdateChecking (custid == c1));
     Commit (tid == i);
     AmalgamateResp true;
     allA ) )

let amalgamateResp = (allA, AmalgamateResp true, allA)

let balanceReq (i : int) (c : int) (bs : int) (bc : int) ?l:(n = (true : [%v: int])) =
  ( (allA;
     OpenAccountsReq (name == n && custid == c);
     allA),
    BalanceReq (name == n),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectSavings (tid == i && custid == c && balance == bs);
     SelectChecking (tid == i && custid == c && balance == bc);
     allA;
     Commit (tid == i); (* TODO: the actual balance? *)
     BalanceResp (balance == (bs + bc));
     allA ) )

let balanceResp = (allA, BalanceResp true, allA)

let depositCheckingReq (i : int) (b : int) (c : int) ?l:(n = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( (allA;
     OpenAccountsReq (name == n && custid == c);
     allA),
    DepositCheckingReq (name == n && amount == a),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectChecking (tid == i && custid == c && balance == b);
     allA;
     UpdateChecking (tid == i && custid == c);(* && balance == (a + b)); *)
     starA (anyA - UpdateChecking (custid == c));
     Commit (tid == i);
     DepositCheckingResp true;
     allA ) )

let depositCheckingResp = (allA, DepositCheckingResp true, allA)

let sendPaymentReq (i : int) (sb : int) (db : int) ?l:(sc = (true : [%v: int])) ?l:(dc = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( (allA;
     OpenAccountsReq (custid == sc || custid == dc);
     allA;
     OpenAccountsReq (custid == sc || custid == dc);
     allA),
    SendPaymentReq (srcid == sc && destid == dc && amount == a),
    (BeginT (tid == i);
     SelectChecking (tid == i && custid == sc && balance == sb);
     SelectChecking (tid == i && custid == dc && balance == db);
     allA;
     UpdateChecking (tid == i && custid == sc && balance == (sb - a));
     UpdateChecking (tid == i && custid == dc && balance == (db + a));
     starA ((anyA - UpdateChecking (custid == sc)) - UpdateChecking (custid == dc));
     Commit (tid == i);
     SendPaymentResp true;
     allA ) )

let sendPaymentResp = (allA, SendPaymentResp true, allA)

let transactSavingsReq (i : int) (b : int) (c : int) ?l:(n = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( (allA;
     OpenAccountsReq (name == n && custid == c);
     allA),
    TransactSavingsReq (name == n && amount == a),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectSavings (tid == i && custid == c && balance == b);
     allA;
     UpdateSavings (tid == i && custid == c && balance == (b + a));
     starA (anyA - UpdateSavings (custid == c));
     Commit (tid == i);
     TransactSavingsResp true;
     allA ) )
    
let transactSavingsResp = (allA, TransactSavingsResp true, allA)

let writeCheckReq (i : int) (c : int) ?l:(n = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( (allA;
     OpenAccountsReq (custid == c && name == n);
     allA),
    WriteCheckReq (name == n && amount == a),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectSavings (tid == i && custid == c);
     SelectChecking (tid == i && custid == c);
     allA;
     UpdateChecking (tid == i && custid == c);
     starA (anyA - UpdateChecking (custid == c));
     Commit (tid == i);
     WriteCheckResp true;
     allA; ) )

let writeCheckResp = (allA, WriteCheckResp true, allA)


(* Global Properties *)
let[@goal] smallbank_cc (c : int) (b : int) =
  allA;
  SelectChecking (custid == c && balance == b);
  starA (anyA - UpdateChecking (custid == c));
  SelectChecking (custid == c && not (balance == b));
  allA
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

val selectAccounts : 
  < tid : int ; prevTid : int ; prevCid : int ; name : int ; custid : int >
[@@obs]

val selectSavings : 
  < tid : int ; prevTid : int ; prevCid : int ; custid : int ; balance : int >
[@@obs]

val selectChecking : 
  < tid : int ; prevTid : int ; prevCid : int ; custid : int ; balance : int >
[@@obs]


val updateAccounts :
  < tid : int ; name : int ; custid : int >
[@@obs]

val updateSavings :
  < tid : int ; custid : int ; balance : int >
[@@obs]

val updateChecking :
  < tid : int ; custid : int ; balance : int >
[@@obs]


val openAccountsReq : < name : int ; custid : int > [@@gen]
val openAccountsResp : < > [@@obs]
val amalgamateReq : < custid0 : int ; custid1 : int > [@@gen]
val amalgamateResp : < > [@@obs]
val balanceReq : < name : int > [@@gen]
val balanceResp : < balance : int > [@@obs]
val depositCheckingReq : < name : int ; amount : int > [@@gen]
val depositCheckingResp : < > [@@obs]
val sendPaymentReq : < sourceid : int ; destid : int ; amount : int > [@@gen]
val sendPaymentResp : < > [@@obs]
val transactSavingsReq : < name : int ; amount : int > [@@gen]
val transactSavingsResp : < > [@@obs]
val writeCheckReq : < name : int ; amount : int > [@@gen]
val writeCheckResp : < > [@@obs]


(* Causal Consistency *)
(* Invariant: For any transaction with tid = i, there does not exist a previous transaction with tid > i. *)
let beginT ?l:(i = (true : [%v: int])) =
  (starA (anyA - BeginT (tid >= i)), BeginT (tid == i), allA)

(* Invariant: Each transaction is committed exactly once; there does not exist a previous commit with cid > j. *)
let commit ?l:(i = (true : [%v: int])) ?l:(j = (true : [%v: int])) =
  ( (starA (anyA - Commit (tid == i || cid >= j));
     BeginT (tid == i);
     starA (anyA - Commit (tid == i || cid >= j))),
    Commit (tid == i && cid == j),
    starA (anyA - UpdateAccounts (tid == i) - SelectAccounts (tid == i)
                - UpdateSavings (tid == i) - SelectSavings (tid == i)
                - UpdateChecking (tid == i) - SelectChecking (tid == i)) )

(* updates *)

let updateAccounts ?l:(i = (true : [%v: int])) ?l:(c = (true : [%v: int]))
    ?l:(n = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateAccounts (tid == i && custid == c && custname == n),
    allA )

let updateSavings ?l:(i = (true : [%v: int])) ?l:(c = (true : [%v: int]))
    ?l:(b = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateSavings (tid == i && custid == c && balance == b),
    allA )

let updateChecking ?l:(i = (true : [%v: int])) ?l:(c = (true : [%v: int]))
    ?l:(b = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    UpdateChecking (tid == i && custid == c&& balance == b),
    allA )


(* selects *)

let selectAccounts =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(c = (true : [%v: int]))
      ?l:(n = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateAccounts (tid == i && custid == c));
         UpdateAccounts (tid == pi && custid == c && custname == n);
         starA (anyA - UpdateAccounts (tid == i && custid == c));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateAccounts (tid == i && custid == c))),
        SelectAccounts
          (tid == i && custid == c && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && custname == n),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

let selectSavings =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(c = (true : [%v: int]))
      ?l:(b = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateSavings (tid == i && custid == c));
         UpdateSavings (tid == pi && custid == c && balance == b);
         starA (anyA - UpdateSavings (tid == i && custid == c));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateSavings (tid == i && custid == c))),
        SelectSavings
          (tid == i && custid == c && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && balance == b),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

let selectChecking =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(c = (true : [%v: int]))
      ?l:(b = (true : [%v: int]))
    ->
      ( (starA (anyA - UpdateChecking (tid == i && custid == c));
         UpdateChecking (tid == pi && custid == c && balance == b);
         starA (anyA - UpdateChecking (tid == i && custid == c));
         Commit (tid == pi && cid == pj);
         starA (anyA - UpdateChecking (tid == i && custid == c))),
        SelectChecking
          (tid == i && custid == c && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && balance == b),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]


(* SmallBank *)
(* TODO: *allowing operations between selects and updates according to causal consistency guidelines
         *fleshing out history and future for operations
         *adding information about balances to these checks
         *adding a goal
         *adding custname to api for some operations *)

let openAccountsReq (i : int) ?l:(n = (true : [%v: int])) ?l:(c = (true : [%v: int])) =
  ( *history*,
    OpenAccountsReq (name == n && custid == c),
    (BeginT (tid == i);
     UpdateAccounts (tid == i && name == n && custid == c);
     UpdateSavings (tid == i && custid == c && balance == 0);
     UpdateChecking (tid == i && custid == c && balance == 0);
     Commit (tid == i);
     OpenAccountsResp true;
     *future* ) )

let openAccountsResp = (allA, OpenAccountsResp true, allA)

let amalgamateReq (i : int) ?l:(c0 = (true : [%v: int])) ?l:(c1 = (true : [%v: int])) =
  ( *history*,
    AmalgamateReq (custid0 == c0 && custid1 == c1),
    (BeginT (tid == i);
     SelectSavings (tid == i && custid == c0);
     SelectChecking (tid == i && custid == c0);
     SelectChecking (tid == i && custid == c1);
     UpdateSavings (tid == i && custid == c0 && balance == 0);
     UpdateChecking (tid == i && custid == c0 && balance == 0);
     UpdateChecking (tid == i && custid == c1);
     Commit (tid == i);
     AmalgamateResp true;
     *future* ) )

let amalgamateResp = (allA, AmalgamateResp true, allA)

let balanceReq (i : int) (c : int) ?l:(n = (true : [%v: int])) =
  ( *history*,
    BalanceReq (custid == c),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectSavings (tid == i && custid == c);
     SelectChecking (tid == i && custid == c);
     BalanceResp true; (* TODO: the actual balance? *)
     *future* ) )

let balanceResp = (allA, BalanceResp true, allA)

let depositCheckingReq (i : int) (b : int) (c : int) ?l:(n = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( *history*,
    DepositCheckingReq (custid == c && amount == a),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectChecking (tid == i && custid == c && balance == b);
     UpdateChecking (tid == i && custid == c && balance == a + b);
     Commit (tid == i);
     DepositCheckingResp true;
     *future* ) )

let depositCheckingResp = (allA, DepositCheckingResp true, allA)

let sendPaymentReq (i : int) (sb : int) (db : int) ?l:(sc = (true : [%v: int])) ?l:(dc = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( *history*,
    SendPaymentReq (srcid == sc && destid == dc && amount == a),
    (BeginT (tid == i);
     SelectChecking (tid == i && custid == sc && balance == sb);
     SelectChecking (tid == i && custid == dc && balance == db);
     UpdateChecking (tid == i && custid == sc && balance == (sb - a));
     UpdateChecking (tid == i && custid == dc && balance == (db + a));
     Commit (tid == i);
     SendPaymentResp true;
     *future* ) )

let sendPaymentResp = (allA, SendPaymentResp true, allA)

let transactSavingsReq (i : int) (b : int) (c : int) ?l:(n = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( *history*,
    TransactSavingsReq (custid == c && amount == a),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectSavings (tid == i && custid == c && balance == b);
     UpdateSavings (tid == i && custid == c && balance == (b + a));
     Commit (tid == i);
     TransactSavingsResp true;
     *future* ) )
    
let transactSavingsResp = (allA, TransactSavingsResp true, allA)

let writeCheckReq (i : int) (c : int) ?l:(n = (true : [%v: int])) ?l:(a = (true : [%v: int])) =
  ( *history*,
    WriteCheckReq (custid == c && amount == a),
    (BeginT (tid == i);
     SelectAccounts (tid == i && name == n && custid == c);
     SelectSavings (tid == i && custid == c);
     SelectChecking (tid == i && custid == c);
     UpdateChecking (tid == i && custid == c);
     Commit (tid == i);
     WriteCheckResp true;
     *future* ) )

let writeCheckResp = (allA, WriteCheckResp true, allA)


(* Global Properties *)
let[@goal] smallbank_cc (u : int) (l : int list) =
  allA;
  SelectFollows (user == u && follows == l);
  starA (anyA - UpdateFollows (user == u));
  SelectFollows (user == u && not (follows == l));
  allA