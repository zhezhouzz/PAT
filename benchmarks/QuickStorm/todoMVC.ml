open Language

type status = Active | Completed

type filterOption = NoneOp | ActiveOp | CompletedOp | AllOp
[@@deriving show, eq, ord]

let parse_filterOption = function
  | "NoneOp" -> NoneOp
  | "ActiveOp" -> ActiveOp
  | "CompletedOp" -> CompletedOp
  | "AllOp" -> AllOp
  | _ -> _die [%here]

module TodoApp = struct
  let opt = ref NoneOp
  let inputBox = ref ""
  let todos = ref []

  let init () =
    opt := NoneOp;
    inputBox := ""

  let setFilter (filter : filterOption) =
    opt := filter;
    inputBox := ""

  let writeInput (todo : string) = inputBox := todo

  let addTodo () =
    todos := (Active, !inputBox) :: !todos;
    inputBox := ""
end

open Interpreter

let display () =
  sendTo
    (None, { op = "displayInput"; args = [ mk_value_string !TodoApp.inputBox ] })

let displayInputHandler (_ : msg) = ()

let initHandler (_ : msg) =
  TodoApp.init ();
  display ()

let setFilterHandler (msg : msg) =
  let filter =
    match msg.ev.args with
    | [ VConst (S filter) ] -> parse_filterOption filter
    | _ -> _die [%here]
  in
  TodoApp.setFilter (filter :> filterOption);
  display ()

let writeInputHandler (msg : msg) =
  let input =
    match msg.ev.args with [ VConst (S input) ] -> input | _ -> _die [%here]
  in
  TodoApp.writeInput input;
  display ()

let addTodoHandler (_ : msg) = TodoApp.addTodo ()

let init () =
  Interpreter.init ();
  register_handler "init" initHandler;
  register_handler "setFilter" setFilterHandler;
  register_handler "writeInput" writeInputHandler;
  register_handler "addTodo" addTodoHandler;
  register_handler "displayInput" displayInputHandler

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "init"#:(record []);
      "setFilter"#:(record [ "filter"#:(mk_p_abstract_ty "filterOption") ]);
      "writeInput"#:(record [ "input"#:(mk_p_abstract_ty "string") ]);
      "addTodo"#:(record []);
      "displayInput"#:(record [ "input"#:(mk_p_abstract_ty "string") ]);
    ]

let genInit = mk_term_gen testCtx "init" []
let genSetFilter filter = mk_term_gen testCtx "setFilter" [ VVar filter ]
let genWriteInput input = mk_term_gen testCtx "writeInput" [ VVar input ]
let genAddTodo = mk_term_gen testCtx "addTodo" []

let obsDisplayInput k =
  mk_term_obs_fresh testCtx "displayInput" (function
    | [ x ] -> k x
    | _ -> _die [%here])

let main =
  (* testAst ();
      let () = _die_with [%here] "done" in *)
  let x = (Rename.unique_var "x")#:(mk_p_abstract_ty "filterOption") in
  let y = (Rename.unique_var "y")#:(mk_p_abstract_ty "filterOption") in
  mk_term_assume [ x ] mk_true
    (mk_term_assume [ y ]
       (Not (lit_to_prop (mk_var_eq_var [%here] x y)))
       (mk_term_assume_fresh_true mk_p_string_ty (fun s ->
            genInit
              (obsDisplayInput (fun _ ->
                   genSetFilter x
                     (obsDisplayInput (fun _ ->
                          genWriteInput s
                            (obsDisplayInput (fun _ ->
                                 genSetFilter y
                                   (obsDisplayInput (fun _ -> mk_term_tt)))))))))))
