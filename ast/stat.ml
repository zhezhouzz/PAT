(* Statistics *)

open ParseTree
open Zdatatype

type automataStat = { num_regex_operator : int; num_sevent : int }
type patStat = { num_gvar : int; num_param : int; automataStat : automataStat }

type complexityStat = {
  num_prim_operators : int;
  num_effect_operators : int;
  num_pat : int;
  num_axioms : int;
  num_syn_goals : int;
  patStat : patStat StrMap.t;
}

let init_complexityStat =
  {
    num_prim_operators = 0;
    num_effect_operators = 0;
    num_pat = 0;
    num_axioms = 0;
    num_syn_goals = 0;
    patStat = StrMap.empty;
  }

let merge_automata_stat s1 s2 =
  {
    num_regex_operator = s1.num_regex_operator + s2.num_regex_operator;
    num_sevent = s1.num_sevent + s2.num_sevent;
  }

let max_automata_stat s1 s2 =
  {
    num_regex_operator = max s1.num_regex_operator s2.num_regex_operator;
    num_sevent = max s1.num_sevent s2.num_sevent;
  }

let max_pat_stat s1 s2 =
  {
    num_gvar = max s1.num_gvar s2.num_gvar;
    num_param = max s1.num_param s2.num_param;
    automataStat = max_automata_stat s1.automataStat s2.automataStat;
  }

open AutomataLibrary

let do_automata_complexity_stat pat =
  let rec aux = function
    | MultiAtomic l -> { num_regex_operator = 0; num_sevent = List.length l }
    | Ctx _ -> { num_regex_operator = 0; num_sevent = 0 }
    | EmptyA | EpsilonA | AnyA -> { num_regex_operator = 1; num_sevent = 0 }
    | CtxOp { body; _ } -> aux body
    | ComplementA r | StarA r | DComplementA { body = r; _ } | RepeatN (_, r) ->
        let stat = aux r in
        { stat with num_regex_operator = stat.num_regex_operator + 1 }
    | LorA (t1, t2) | LandA (t1, t2) | SetMinusA (t1, t2) ->
        merge_automata_stat (aux t1) (aux t2)
    | SeqA l ->
        List.fold_left
          (fun stat t -> merge_automata_stat stat (aux t))
          { num_regex_operator = 0; num_sevent = 0 }
          l
  in
  aux pat

let do_pat_complexity_stat pat =
  let rec aux = function
    | RtyBase _ -> _die [%here]
    | RtyHAParallel _ -> _die [%here]
    | RtyGArr { retrty; _ } ->
        let stat = aux retrty in
        { stat with num_gvar = stat.num_gvar + 1 }
    | RtyArr { retrty; _ } ->
        let stat = aux retrty in
        { stat with num_param = stat.num_param + 1 }
    | RtyInter (t1, t2) -> max_pat_stat (aux t1) (aux t2)
    | RtyHAF { history; adding; future } ->
        let stat_history = do_automata_complexity_stat history in
        let stat_adding = do_automata_complexity_stat adding in
        let stat_future = do_automata_complexity_stat future in
        let automataStat =
          merge_automata_stat
            (merge_automata_stat stat_history stat_adding)
            stat_future
        in
        { num_gvar = 0; num_param = 0; automataStat }
  in
  aux pat

let do_complexity_stat items =
  let aux stat = function
    | PrimDecl _ ->
        { stat with num_prim_operators = stat.num_prim_operators + 1 }
    | MsgNtDecl _ ->
        { stat with num_effect_operators = stat.num_effect_operators + 1 }
    | MsgDecl { name; pat } ->
        {
          stat with
          num_pat = stat.num_pat + 1;
          patStat = StrMap.add name (do_pat_complexity_stat pat) stat.patStat;
        }
    | SynGoal _ -> { stat with num_syn_goals = stat.num_syn_goals + 1 }
    | PrAxiom _ -> { stat with num_axioms = stat.num_axioms + 1 }
  in
  aux init_complexityStat items

let output_complexity_stat stat =
  let num_op = stat.num_effect_operators in
  let num_qualifier =
    StrMap.fold
      (fun _ x acc -> acc + x.automataStat.num_regex_operator)
      stat.patStat 0
  in
  (num_op, num_qualifier)
