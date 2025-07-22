(* (* lib/codegen.ml *)
open Ast

let generate_code (prog : program) : string =
  let buffer = Buffer.create 1024 in
  (* 目前我们只处理程序中的第一个函数 *)
  let main_func = List.hd prog in
  (* 生成汇编代码头部 *)
  Buffer.add_string buffer ".globl main\n";
  Buffer.add_string buffer "main:\n";
  (* 函数序言 (Function Prologue) *)
  Buffer.add_string buffer "  push %rbp\n";
  Buffer.add_string buffer "  mov %rsp, %rbp\n";
  (* 遍历函数体内的语句 *)
  List.iter
    (fun statement ->
       match statement with
       | Return expr ->
         (match expr with
          | IntLiteral n ->
            (* 将返回值放入 %eax 寄存器 *)
            Buffer.add_string buffer (Printf.sprintf "  mov $%d, %%eax\n" n)
          | _ -> failwith "Code generation for this expression is not implemented yet")
       | _ -> failwith "Code generation for this statement is not implemented yet")
    main_func.body;
  (* 函数尾声 (Function Epilogue) *)
  Buffer.add_string buffer "  pop %rbp\n";
  Buffer.add_string buffer "  ret\n";
  Buffer.contents buffer
;;
*)
(* codegen.ml *)
(* • 分离临时与标签计数器
    • 布尔、比较、短路逻辑归一化为 0/1
    • 调用序列 param / call / retval
    假设物理寄存器6个
--------------------------------------------------------------*)

open Ast
open Semantic

(* ---------- 计数器 ---------- *)
let temp_counter = ref 0
let label_counter = ref 0

let fresh_tmp () =
  let t = "t" ^ string_of_int !temp_counter in
  incr temp_counter;
  t
;;

let fresh_lbl pfx =
  let l = pfx ^ string_of_int !label_counter in
  incr label_counter;
  l
;;

exception SemanticError of string (* 查符号失败时抛出 *)

(* 把语义阶段的符号信息打包成 codegen 用的 env *)
type cg_env =
  { vars : typ Semantic.VarEnv.t
  ; funcs : func_sig Semantic.FuncEnv.t
  }

let make_env ~(globals : func_sig FuncEnv.t) ~(locals : typ VarEnv.t) : cg_env =
  { vars = locals; funcs = globals }
;;

(* ============================================================= *)
(*  Expr → code * result tmp                                      *)
(* ============================================================= *)

let rec gen_expr env (e : expr) : string list * string =
  match e with
  | IntLiteral n ->
    let t = fresh_tmp () in
    [ Printf.sprintf "%s = %d" t n ], t
  | Id x ->
    (match VarEnv.find_opt x env.vars with
     | None -> raise (SemanticError ("unbound variable " ^ x))
     | Some _ ->
       let t = fresh_tmp () in
       [ Printf.sprintf "%s = %s" t x ], t)
  | Assign (x, rhs) ->
    if VarEnv.find_opt x env.vars = None
    then raise (SemanticError ("assignment to undeclared " ^ x));
    let c_rhs, t_rhs = gen_expr env rhs in
    let t_ret = fresh_tmp () in
    c_rhs @ [ Printf.sprintf "%s = %s" x t_rhs; Printf.sprintf "%s = %s" t_ret x ], t_ret
  | BinOp (op, e1, e2) when List.mem op [ Add; Sub; Mul; Div; Mod ] ->
    let c1, t1 = gen_expr env e1 in
    let c2, t2 = gen_expr env e2 in
    let t = fresh_tmp () in
    let op_str =
      match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | _ -> assert false
    in
    c1 @ c2 @ [ Printf.sprintf "%s = %s %s %s" t t1 op_str t2 ], t
  | BinOp (op, e1, e2) when List.mem op [ Eq; Neq; Lt; Le; Gt; Ge ] ->
    let c1, t1 = gen_expr env e1 in
    let c2, t2 = gen_expr env e2 in
    let t = fresh_tmp () in
    let l_true = fresh_lbl "L_true_" in
    let l_end = fresh_lbl "L_end_" in
    let br =
      match op with
      | Eq  -> "beq" | Neq -> "bne" | Lt  -> "blt"
      | Le  -> "ble" | Gt  -> "bgt" | Ge  -> "bge" | _ -> assert false
    in
    ( c1 @ c2
      @ [ Printf.sprintf "%s = 0" t
        ; Printf.sprintf "%s %s, %s, %s" br t1 t2 l_true
        ; Printf.sprintf "j %s" l_end
        ; l_true ^ ":"
        ; Printf.sprintf "%s = 1" t
        ; l_end ^ ":"
        ]
    , t )
  | BinOp (op, e1, e2) when op = And || op = Or ->
    let c1, t1 = gen_expr env e1 in
    let c2, t2 = gen_expr env e2 in
    let t_res = fresh_tmp () in
    let l_short = fresh_lbl "L_short_" in
    let l_end = fresh_lbl "L_end_" in
    let set_short, set_normal, cond1, cond2 =
      if op = And then
        ( "0"
        , "1"
        , Printf.sprintf "beqz %s, %s" t1 l_short
        , Printf.sprintf "beqz %s, %s" t2 l_short )
      else
        ( "1"
        , "0"
        , Printf.sprintf "bnez %s, %s" t1 l_short
        , Printf.sprintf "bnez %s, %s" t2 l_short )
    in
    ( c1
      @ [ cond1 ]
      @ c2
      @ [ cond2
        ; Printf.sprintf "%s = %s" t_res set_normal
        ; Printf.sprintf "j %s" l_end
        ; l_short ^ ":"
        ; Printf.sprintf "%s = %s" t_res set_short
        ; l_end ^ ":"
        ]
    , t_res )
  | UnOp (Not, e1) ->
    let c, t1 = gen_expr env e1 in
    let t = fresh_tmp () in
    let l_true = fresh_lbl "L_true_" in
    let l_end = fresh_lbl "L_end_" in
    ( c
      @ [ Printf.sprintf "%s = 0" t
        ; Printf.sprintf "beqz %s, %s" t1 l_true
        ; Printf.sprintf "j %s" l_end
        ; l_true ^ ":"
        ; Printf.sprintf "%s = 1" t
        ; l_end ^ ":"
        ]
    , t )
  | UnOp (Neg, e1) ->
    let c, t1 = gen_expr env e1 in
    let t = fresh_tmp () in
    c @ [ Printf.sprintf "%s = - %s" t t1 ], t
  | Call (fname, args) ->
    (match FuncEnv.find_opt fname env.funcs with
     | None -> raise (SemanticError ("unknown function " ^ fname))
     | Some sig_ ->
       if List.length args <> List.length sig_.params
       then raise (SemanticError ("arity mismatch in call " ^ fname));
       let arg_codes, arg_tmps = List.split (List.map (gen_expr env) args) in
       let t_ret = fresh_tmp () in
       ( List.flatten arg_codes
         @ List.map (fun t -> "param " ^ t) arg_tmps
         @ [ Printf.sprintf "call %s, %d" fname (List.length arg_tmps);
            Printf.sprintf "%s = retval" t_ret
           ]
       , t_ret))
  | _ -> raise (SemanticError "unsupported expr")
;;

(* ============================================================= *)
(*  Stmt → code list                                              *)
(* ============================================================= *)

let rec gen_stmt env ?break_lbl ?cont_lbl (s : stmt) : string list =
  match s with
  | Block lst -> List.flatten (List.map (gen_stmt env ?break_lbl ?cont_lbl) lst)
  | VarDecl (id, init_e) ->
    if VarEnv.mem id env.vars then raise (SemanticError ("redefinition of " ^ id));
    let c, t = gen_expr env init_e in
    c @ [ Printf.sprintf "%s = %s" id t ]
  | Expr e -> fst (gen_expr env e)
  | Return None -> [ "ret 0" ]
  | Return (Some e) ->
    let c, t = gen_expr env e in
    c @ [ Printf.sprintf "ret %s" t ]
  | If (cond, s_then, s_else_opt) ->
    let c_cond, t_c = gen_expr env cond in
    let l_else = fresh_lbl "L_else_" in
    let l_end = fresh_lbl "L_end_" in
    let then_c = gen_stmt env ?break_lbl ?cont_lbl s_then in
    let else_c = match s_else_opt with Some s -> gen_stmt env ?break_lbl ?cont_lbl s | None -> [] in
    c_cond
    @ [ Printf.sprintf "beqz %s, %s" t_c l_else ]
    @ then_c
    @ [ Printf.sprintf "j %s" l_end; l_else ^ ":" ]
    @ else_c
    @ [ l_end ^ ":" ]
  | While (cond, body) ->
    let l_begin = fresh_lbl "L_begin_" in
    let l_end = fresh_lbl "L_end_" in
    let c_cond, t_c = gen_expr env cond in
    let body_c = gen_stmt env ~break_lbl:l_end ~cont_lbl:l_begin body in
    [ l_begin ^ ":" ]
    @ c_cond
    @ [ Printf.sprintf "beqz %s, %s" t_c l_end ]
    @ body_c
    @ [ Printf.sprintf "j %s" l_begin; l_end ^ ":" ]
  | Break -> (match break_lbl with Some l -> [ Printf.sprintf "j %s" l ] | None -> raise (SemanticError "break not in loop"))
  | Continue -> (match cont_lbl with Some l -> [ Printf.sprintf "j %s" l ] | None -> raise (SemanticError "continue not in loop"))
  | _ -> []
;;

(* ============================================================= *)
(*  Function / Program                                            *)
(* ============================================================= *)

let gen_func (ana : analysis_result) (f : func_def) : string list =
  let locals =
    try List.assoc f.fname ana.local_vars
    with Not_found -> raise (Failure ("internal: no locals for " ^ f.fname))
  in
  let env = make_env ~globals:ana.global_funcs ~locals in
  (f.fname ^ ":") :: List.flatten (List.map (gen_stmt env) f.body)
;;

let gen_program (p : program) : string list =
  let ana = Semantic.analyze_program p in
  List.flatten (List.map (gen_func ana) p)
;;

(* ------------------------------------------------------------------ *)
(* Graph Coloring Register Allocation with Spill                    *)
(* ------------------------------------------------------------------ *)
module SS = Set.Make(String)
module SM = Map.Make(String)

let analyze_liveness ir =
  List.fold_left (fun s instr ->
    List.fold_left (fun s tok -> if tok <> "=" then SS.add tok s else s) s (String.split_on_char ' ' instr)
  ) SS.empty ir

let build_interference temps =
  let g = Hashtbl.create (List.length temps) in
  List.iter (fun v -> Hashtbl.add g v SS.empty) temps;
  List.iter (fun v1 ->
    List.iter (fun v2 -> if v1 <> v2 then
      let s1 = Hashtbl.find g v1 in Hashtbl.replace g v1 (SS.add v2 s1)
    ) temps
  ) temps;
  g

let simplify g phys stack =
  let tbl = Hashtbl.copy g in
  try while Hashtbl.length tbl > 0 do
    let v = Hashtbl.fold (fun v adj acc -> if SS.cardinal adj < List.length phys then v else acc) tbl "" in
    Hashtbl.remove tbl v;
    Stack.push v stack;
    Hashtbl.iter (fun key adj -> Hashtbl.replace tbl key (SS.remove v adj)) tbl
  done with _ -> ()

let select stack g phys =
  let mapping = Hashtbl.create 16 in
  while not (Stack.is_empty stack) do
    let v = Stack.pop stack in
    let adj = Hashtbl.find g v in
    let used = SS.fold (fun u acc -> match Hashtbl.find_opt mapping u with Some r->r::acc|None->acc) adj [] in
    match List.find_opt (fun r -> not (List.mem r used)) phys with
    | Some reg -> Hashtbl.add mapping v reg
    | None -> failwith ("Spill needed for variable " ^ v)
  done;
  mapping

let allocate_registers ir =
  let phys = ["t0";"t1";"t2";"t3";"t4";"t5"] in
  let temps = SS.elements (analyze_liveness ir) in
  let ig = build_interference temps in
  let stack = Stack.create () in
  simplify ig phys stack;
  let mapping = select stack ig phys in
  List.map (fun instr ->
    instr
    |> String.split_on_char ' '
    |> List.map (fun tok -> try Hashtbl.find mapping tok with Not_found -> tok)
    |> String.concat " ")
    ir

(* ------------------------------------------------------------------ *)
(* Assembly Generation                                                *)
(* ------------------------------------------------------------------ *)
let assemble_instr instr =
  let ws = String.split_on_char ' ' instr in
  match ws with
  | [dst;"=";s1;"+";s2] -> Printf.sprintf "  add %s, %s, %s" dst s1 s2
  | [dst;"=";s1;"-";s2] -> Printf.sprintf "  sub %s, %s, %s" dst s1 s2
  | [dst;"=";s1;"*";s2] -> Printf.sprintf "  mul %s, %s, %s" dst s1 s2
  | [dst;"=";s1;"/";s2] -> Printf.sprintf "  div %s, %s, %s" dst s1 s2
  | ["param";r] -> Printf.sprintf "  mv a0, %s" r
  | ["call";f;_] -> Printf.sprintf "  call %s" f
  | ["ret";r] -> Printf.sprintf "  mv a0, %s\n  ret" r
  | [lbl] when String.get lbl (String.length lbl - 1) = ':' -> lbl
  | ["j";lbl] -> Printf.sprintf "  j %s" lbl
  | _ -> failwith ("unhandled: " ^ instr)

let gen_prologue () = [".text"; ".globl main"; "main:"]
let gen_epilogue () = ["  ret"]

let gen_assembly ir =
  let colored = allocate_registers ir in
  gen_prologue () @ List.map assemble_instr colored @ gen_epilogue ()

(* ------------------------------------------------------------------ *)
(* Public Interface                                                   *)
(* ------------------------------------------------------------------ *)
let compile_source src =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.token lexbuf in
  let ir = gen_program ast in
  gen_assembly ir
