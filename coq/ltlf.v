From Coq Require Import Lists.List.
Import ListNotations.
Require Import Program Arith.
Require Import Coq.Logic.Classical_Prop.

Definition trace {A: Type} := list A.
Definition ap {A: Type} := A -> Prop.

Inductive ltlf {A: Type}: Type :=
| atom: (A -> Prop) -> ltlf
(* | next: ltlf -> ltlf *)
(* | eventual: ltlf -> ltlf *)
| global: ltlf -> ltlf
| neg: ltlf -> ltlf.

Fixpoint size_ltlf {A: Type} (l: @ltlf A) :=
  match l with
  | atom _ => 0
  (* | eventual l => 1 + size_ltlf l *)
  | global l => 1 + size_ltlf l
  | neg l => 1 + size_ltlf l
  end.

Program Fixpoint denot {A: Type} (l: @ltlf A) (tr: list A) { measure ((size_ltlf l) + (length tr)) } : Prop :=
  match l with
  | atom p =>
      (match tr with
      | [] => False
      | a :: _ => p a
      end)
  | global l =>
      (match tr with
       | [] => True
       | _ :: tr => denot l tr /\ denot (global l) tr
       end)
  | neg l => ~ (denot l tr)
  end.
Obligation 1.
Proof.
  simpl. intuition.
Qed.
Obligation 2.
Proof.
  simpl. intuition.
Qed.

Lemma denot_atom {A: Type}: forall (a: A) (tr: list A) (p: A -> Prop), p a <-> @denot A (atom p) (a :: tr).
Proof.
  intros. cbv. intuition.
Qed.

(* Inductive denot {A: Type}: @ltlf A -> list A -> Prop := *)
(* | denot_atom: forall (a: A) (tr: list A) (p: A -> Prop), p a -> denot (atom p) (a :: tr) *)
(* | denot_global1: forall (l : ltlf), denot (global l) [] *)
(* | denot_global2: forall (a: A) (tr: list A) (l : ltlf), denot l (a :: tr) -> denot (global l) tr -> denot (global l) (a :: tr) *)
(* | denot_eventual1: forall (a: A) (tr: list A) (l : ltlf), denot l (a :: tr) -> denot (eventual l) (a :: tr) *)
(* | denot_eventual2: forall (a: A) (tr: list A) (l : ltlf), denot (eventual l) tr -> denot (eventual l) (a :: tr) *)
(* | denot_neg: forall (tr: list A) (l: ltlf), ~ denot l tr -> denot (neg l) tr. *)

Definition top {A: Type} := fun (_:A) => True.
Definition bot {A: Type} := fun (_:A) => False.

Lemma denot_top {A: Type}: forall tr: list A, denot (atom top) tr <-> ~ (tr = []).
Proof.
  split; simpl; intros; destruct tr.
  - cbv in H. inversion H.
  - intuition. inversion H0.
  - intuition.
  - cbv. intuition.
Qed.

Lemma denot_bot {A: Type}: forall tr: list A, denot (atom bot) tr -> False.
Proof.
  intros. cbv in H. destruct tr; intuition.
Qed.

Lemma denot_neg_top {A: Type}: forall tr: list A, denot (neg (atom top)) tr <-> (tr = []).
Proof.
  intros. cbv. destruct tr; intuition. inversion H.
Qed.

Lemma denot_neg_bot {A: Type}: forall tr: list A, denot (neg (atom bot)) tr.
Proof.
  intros. cbv. destruct tr; intuition.
Qed.

Lemma denot_global_top {A: Type}: forall tr: list A, @denot A (global (atom top)) tr.
Proof.
  induction tr.
  - intros. cbv. intuition.
  - intros. cbv.
destruct tr.
  destruct H.
  . cbv in H. destruct tr; intuition. inversion H.
Qed.

Lemma denot_eventual_neg_top {A: Type}: @denot A (global (atom top))) [].
Proof.
  cbv. left. intros. inversion H.
Qed.

Lemma denot_eventual_neg_top {A: Type}: forall tr: list A, denot (eventual (neg (atom top))) tr -> False.
Proof.
  intros. remember (neg (atom top)) as K. cbv beta delta in H.
  destruct H.
  . cbv in H. destruct tr; intuition. inversion H.
Qed.

Lemma denot_neg_bot {A: Type}: forall tr: list A, denot (neg (atom bot)) tr.
Proof.
  intros. cbv. destruct tr; intuition.
Qed.

Lemma denot_global_top {A: Type}: forall tr: list A, denot (global (atom top)) tr.
Proof.
  intros. cbv in H. destruct tr; intuition.
Qed.

Lemma denot_neg {A: Type}: forall (tr: list A) (l: ltlf), ~ denot l tr -> denot (neg l) tr.
Proof.
  intros. unfold denot. unfold denot_func. unfold Fix_sub. unfold Fix_F_sub. 
simpl.
  cbv. intros. intuition.

Lemma denot_neg {A: Type}: forall l (tr: list A), denot l tr <-> ~ (denot (neg l) tr).
Proof.
  induction l; split; intros.
  - intro. cbv in H0. intuition.
  - cbv in H. destruct tr; intuition. cbv. destruct (classic (P a)); intuition.
  - intro. cbv in H0. intuition.
    cbv. intuition.
  split; intros.
  - inversion H; subst. intro HC. inversion HC.
  - destruct tr.
    + intuition.
    + econstructor. unfold top. intuition.
Qed.
