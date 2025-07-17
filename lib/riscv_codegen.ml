open Ast

(* Unique label generator for codegen *)
let label_id = ref 0
let fresh_label base =
  let id = !label_id in
  incr label_id;
  Printf.sprintf ".L_%s_%d" base id

(* ----- 语义分析环境定义 ----- *)
type vinfo = {
  v_ty: [`Int];
  initialized: bool;
}

type fsign = {
  ret_ty: [ `Int | `Void ];
  params: string list;
}

let global_funcs : func list ref = ref []

type senv = {
  vars: (string, vinfo) Hashtbl.t;
  parent: senv option;
  breakable: bool;
  cur_func: fsign option;
}

let rec find_var env name =
  match Hashtbl.find_opt env.vars name with
  | Some v -> Some v
  | None ->
    (match env.parent with
    | Some p -> find_var p name
    | None -> None)

let check_func_signatures (funcs:func list) =
  let tbl = Hashtbl.create 17 in
  List.iter (fun f ->
    if Hashtbl.mem tbl f.name then
      failwith ("Function redefined: " ^ f.name);
    Hashtbl.add tbl f.name true
  ) funcs;
  if not (Hashtbl.mem tbl "main") then
    failwith "No main function"
  else
    match List.find (fun f -> f.name="main") funcs with
    | {ret_ty=`Int; params=[];_} -> ()
    | _ -> failwith "main should be 'int main()'"

let rec analyze_expr env = function
  | Int _ -> `Int
  | Var x ->
      (match find_var env x with
      | Some {v_ty=`Int;initialized=true} -> `Int
      | Some {initialized=false;_} -> failwith ("Variable not initialized: " ^ x)
      | None -> failwith ("Variable not declared: " ^ x))
  | BinOp (_, a, b) ->
      (match analyze_expr env a, analyze_expr env b with
      | `Int, `Int -> `Int
      | _ -> failwith "Binary op requires int operands")
  | UnOp (_, e) ->
      (match analyze_expr env e with
      | `Int -> `Int
      | _ -> failwith "Unary op requires int operand")
  | Call (fname, args) ->
      let funcs = !global_funcs in
      (match List.find_opt (fun f -> f.name = fname) funcs with
      | None -> failwith ("Function not defined: " ^ fname)
      | Some f ->
          if List.length args <> List.length f.params then
            failwith (Printf.sprintf "Function '%s' expects %d args" fname (List.length f.params));
          List.iter (fun arg -> 
            match analyze_expr env arg with
            | `Int -> ()
            | _ -> failwith "Function arguments must be int"
          ) args;
          f.ret_ty)

and analyze_stmt env = function
  | Empty -> false
  | Expr e -> ignore(analyze_expr env e); false
  | Assign (x, e) ->
      (match find_var env x with
      | Some v when v.v_ty=`Int ->
          ignore(analyze_expr env e);
          Hashtbl.replace env.vars x {v_ty=`Int;initialized=true}
      | _ -> failwith ("Assign to undeclared var: " ^ x)); false
  | Decl (x, e) ->
      if Hashtbl.mem env.vars x then failwith ("Duplicated var: " ^ x);
      ignore(analyze_expr env e);
      Hashtbl.add env.vars x {v_ty=`Int;initialized=true}; false
  | Block stmts ->
      let new_env = {
        vars=Hashtbl.create 32;
        parent=Some env;
        breakable=env.breakable;
        cur_func=env.cur_func
      } in
      analyze_stmts new_env stmts
  | If (cond, s_then, Some s_else) ->
      ignore(analyze_expr env cond);
      let r1 = analyze_stmt env s_then in
      let r2 = analyze_stmt env s_else in
      r1 && r2
  | If (cond, s_then, None) ->
      ignore(analyze_expr env cond);
      ignore(analyze_stmt env s_then); false
  | While (cond, body) ->
      ignore(analyze_expr env cond);
      let new_env = {env with breakable=true} in
      ignore(analyze_stmt new_env body); false
  | Break ->
      if not env.breakable then failwith "break not in loop"; false
  | Continue ->
      if not env.breakable then failwith "continue not in loop"; false
  | Return None ->
      (match env.cur_func with
      | Some {ret_ty=`Int;_} -> failwith "must return int"
      | Some {ret_ty=`Void;_} -> true
      | None -> failwith "not in function")
  | Return (Some e) ->
      (match env.cur_func with
      | Some {ret_ty=`Int;_} ->
          let ty = analyze_expr env e in
          if ty<>`Int then failwith "return type mismatch";
          true
      | Some {ret_ty=`Void;_} -> failwith "void func can't return value"
      | None -> failwith "not in function")

and analyze_stmts env stmts =
  let returns = List.map (analyze_stmt env) stmts in
  List.exists (fun x -> x) returns

let analyze_func (f : func) =
  let env = {
    vars = Hashtbl.create 32;
    parent = None;
    breakable = false;
    cur_func = Some { ret_ty = f.ret_ty; params = f.params }
  } in
  List.iter (fun name ->
    Hashtbl.add env.vars name { v_ty = `Int; initialized = true }
  ) f.params;
  let has_return = analyze_stmts env f.body in
  match f.ret_ty with
  | `Int -> if not has_return then failwith (f.name ^ " not all paths return int")
  | `Void -> ()

let analyze (prog: program) =
  global_funcs := prog;
  check_func_signatures prog;
  List.iter analyze_func prog

(* ----- 代码生成部分 ----- *)
let word_size = 4

type cenv = {
  stack_offset : (string, int) Hashtbl.t;
  mutable cur_offset : int;
  parent : cenv option;
  break_label : string option;
  continue_label : string option;
  mutable ret_label : string;
}

let push_env () = { stack_offset=Hashtbl.create 16; cur_offset=0; parent=None; break_label=None; continue_label=None; ret_label="" }

let push_block_env parent_env = {
  stack_offset = Hashtbl.create 16;
  cur_offset = parent_env.cur_offset;
  parent = Some parent_env;
  break_label = parent_env.break_label;
  continue_label = parent_env.continue_label;
  ret_label = parent_env.ret_label;
}

let alloc_var env name =
  env.cur_offset <- env.cur_offset - word_size;
  Hashtbl.add env.stack_offset name env.cur_offset;
  env.cur_offset

let rec find_var env name =
  match Hashtbl.find_opt env.stack_offset name with
  | Some ofs -> ofs
  | None ->
    match env.parent with
    | Some p -> find_var p name
    | None -> failwith ("undefined variable: " ^ name)

let reg_tmp = [|"t0";"t1";"t2";"t3";"t4";"t5";"t6"|]

let rec gen_expr env depth e =
  match e with
  | Int n ->
      let reg = reg_tmp.(depth mod Array.length reg_tmp) in
      (reg, [Printf.sprintf "li %s, %d" reg n])
  | Var x ->
      let reg = reg_tmp.(depth mod Array.length reg_tmp) in
      let ofs = find_var env x in
      (reg, [Printf.sprintf "lw %s, %d(fp)" reg ofs])
  | BinOp (op,a,b) ->
      let reg1, code1 = gen_expr env depth a in
      let reg2, code2 = gen_expr env (depth+1) b in
      let reg = reg_tmp.(depth mod Array.length reg_tmp) in
let opstr, extra =
  match op with
  | Add -> "add", ""
  | Sub -> "sub", ""
  | Mul -> "mul", ""
  | Div -> "div", ""
  | Mod -> "rem", ""
  | Lt  -> "slt", ""
  | Gt  -> "sgt", ""
  | Le  -> "slt", Printf.sprintf "\n\txori %s, %s, 1" reg reg  (* a <= b 实现为 !(b < a) *)
  | Ge  -> "slt", Printf.sprintf "\n\txori %s, %s, 1" reg reg  (* a >= b 实现为 !(a < b) *)
  | Eq  -> "sub", Printf.sprintf "\n\tseqz %s, %s" reg reg
  | Neq -> "sub", Printf.sprintf "\n\tsnez %s, %s" reg reg
  | And -> "and", ""
  | Or  -> "or", ""
      in
      let code =
        code1 @ code2 @
        [Printf.sprintf "%s %s, %s, %s%s"
          opstr reg reg1 reg2 extra]
      in
      (reg, code)
  | UnOp (Neg, e) ->
      let reg, code = gen_expr env depth e in
      (reg, code @ [Printf.sprintf "neg %s, %s" reg reg])
  | UnOp (Not, e) ->
      let reg, code = gen_expr env depth e in
      (reg, code @ [Printf.sprintf "seqz %s, %s" reg reg])
  | UnOp (Pos, e) ->
      let reg, code = gen_expr env depth e in
      (reg, code)
  | Call (fname, args) ->
      let code = ref [] in
      List.iteri (fun i arg ->
        let reg, c = gen_expr env depth arg in
        code := !code @ c @ [Printf.sprintf "mv a%d, %s" i reg]
      ) args;
      code := !code @ [Printf.sprintf "call %s" fname];
      ("a0", !code)

let rec gen_stmt env depth code s =
  match s with
  | Empty -> code
  | Expr e ->
      let _, c = gen_expr env depth e in
      code @ c
  | Assign (x, e) ->
      let reg, c = gen_expr env depth e in
      let ofs = find_var env x in
      code @ c @ [Printf.sprintf "sw %s, %d(fp)" reg ofs]
  | Decl (x, e) ->
      let ofs = alloc_var env x in
      let reg, c = gen_expr env depth e in
      code @ c @ [Printf.sprintf "sw %s, %d(fp)" reg ofs]
  | Block ss ->
      let block_env = push_block_env env in
      List.fold_left (fun acc s -> gen_stmt block_env depth acc s) code ss
  | If (cond, s_then, Some s_else) ->
      let l_else = fresh_label "else" in
      let l_end = fresh_label "endif" in
      let reg, ccond = gen_expr env depth cond in
      let c_then = gen_stmt env depth [] s_then in
      let c_else = gen_stmt env depth [] s_else in
      code @ ccond @
      [Printf.sprintf "beqz %s, %s" reg l_else]
      @ c_then @ [Printf.sprintf "j %s" l_end; Printf.sprintf "%s:" l_else]
      @ c_else @ [Printf.sprintf "%s:" l_end]
  | If (cond, s_then, None) ->
      let l_end = fresh_label "endif" in
      let reg, ccond = gen_expr env depth cond in
      let c_then = gen_stmt env depth [] s_then in
      code @ ccond @
      [Printf.sprintf "beqz %s, %s" reg l_end]
      @ c_then @ [Printf.sprintf "%s:" l_end]
  | While (cond, body) ->
      let l_cond = fresh_label "while_cond" in
      let l_end = fresh_label "while_end" in
      let l_body = fresh_label "while_body" in
      let reg, ccond = gen_expr env depth cond in
      let env' = { env with break_label=Some l_end; continue_label=Some l_cond } in
      let c_body = gen_stmt env' depth [] body in
      code @
      [Printf.sprintf "%s:" l_cond] @ ccond
      @ [Printf.sprintf "beqz %s, %s" reg l_end]
      @ [Printf.sprintf "%s:" l_body] @ c_body
      @ [Printf.sprintf "j %s" l_cond; Printf.sprintf "%s:" l_end]
  | Break ->
      (match env.break_label with
      | Some l -> code @ [Printf.sprintf "j %s" l]
      | None -> failwith "break not in loop")
  | Continue ->
      (match env.continue_label with
      | Some l -> code @ [Printf.sprintf "j %s" l]
      | None -> failwith "continue not in loop")
  | Return None ->
      code @ [Printf.sprintf "j %s" env.ret_label]
  | Return (Some e) ->
      let reg, c = gen_expr env depth e in
      code @ c @ [Printf.sprintf "mv a0, %s" reg; Printf.sprintf "j %s" env.ret_label]

let gen_func (f : func) =
  let env = push_env () in
  let prologue = ref [] in
  (* 参数分配 *)
  List.iteri (fun i name ->
    let ofs = alloc_var env name in
    prologue := !prologue @ [Printf.sprintf "sw a%d, %d(fp)" i ofs]
  ) f.params;
  let ret_label = fresh_label (f.name ^ "_ret") in
  env.ret_label <- ret_label;
  let body_code = List.fold_left (fun acc s -> gen_stmt env 0 acc s) [] f.body in
  let stack_size = -env.cur_offset + 16 in
  let code = ref [] in
  code := !code @ [Printf.sprintf ".globl %s" f.name; Printf.sprintf "%s:" f.name];
  code := !code @ [
    Printf.sprintf "addi sp, sp, -%d" stack_size;
    "sw ra, 0(sp)";
    "sw fp, 4(sp)";
    Printf.sprintf "addi fp, sp, %d" (stack_size)
  ];
  code := !code @ !prologue;
  code := !code @ body_code;
  code := !code @ [Printf.sprintf "%s:" ret_label];
  code := !code @ [
    "lw ra, 0(sp)";
    "lw fp, 4(sp)";
    Printf.sprintf "addi sp, sp, %d" stack_size;
    "ret"
  ];
  String.concat "\n" !code

let gen_program (prog : program) : string =
  String.concat "\n\n" (List.map gen_func prog)