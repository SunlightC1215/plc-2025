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
let rec ends_with_return stmt =
  match stmt with
  | Return _ -> true
  | Block stmts ->
    (match List.rev stmts with
     | [] -> false
     | last :: _ -> ends_with_return last)
  | If (_, then_blk, Some else_blk) ->
      (* 只有 if-else 的两个分支都以 Return 结尾时才返回 true *)
      ends_with_return then_blk && ends_with_return else_blk
  | If (_, then_blk, None) ->
      ends_with_return then_blk
  | _ -> false  (* 其他语句不会导致函数返回 *)
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
let reg_tmp = [|"t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"|]
let max_arg_regs = 8
let word = word_size


let tmp_reg_of_depth depth =
  reg_tmp.(depth mod Array.length reg_tmp)

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
  stack_offset = Hashtbl.create 16;          (* 只复制名字表 *)
  cur_offset = parent_env.cur_offset;       (* 初始值拷贝，后面会回写 *)
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


let rec gen_expr env depth e =
  match e with
  | Int n ->
      let reg = tmp_reg_of_depth depth in
      (reg, [Printf.sprintf "li %s, %d" reg n])

  | Var x ->
      let reg = tmp_reg_of_depth depth in
      let ofs = find_var env x in
      (reg, [Printf.sprintf "lw %s, %d(fp)" reg ofs])

  (* ----------------------------------------------------------- *)
  (* 二元运算：左值先保存，右值求完后再恢复左值                *)
  (* ----------------------------------------------------------- *)
  | BinOp (op, a, b) ->
    (* ① 生成左子表达式 *)
    let left_reg, left_code = gen_expr env depth a in

    (* ② 若左子结果恰好在 a0（函数返回值），先搬到临时寄存器 *)
    let left_reg', left_move_code =
      if left_reg = "a0" then
        let tmp = tmp_reg_of_depth (depth + 100) in
        (tmp, [Printf.sprintf "mv %s, a0" tmp])
      else (left_reg, []) in

    (* ③ 保存左子值到栈（16 B 对齐） *)
    let save_area = 16 in
    let push_code = [
      Printf.sprintf "addi sp, sp, -%d" save_area;
      Printf.sprintf "sw %s, 0(sp)" left_reg'
    ] in

    (* ④ 生成右子表达式 *)
    let right_reg, right_code = gen_expr env (depth + 1) b in

    (* ⑤ 弹回左子值到 **另一个**临时寄存器 *)
    let left_back_reg = tmp_reg_of_depth (depth + 200) in
    let pop_code = [
      Printf.sprintf "lw %s, 0(sp)" left_back_reg;
      Printf.sprintf "addi sp, sp, %d" save_area
    ] in
      (* 5️⃣ 选取对应的指令 *)
      let opstr, extra =
        match op with
        | Add -> "add", ""
        | Sub -> "sub", ""
        | Mul -> "mul", ""
        | Div -> "div", ""
        | Mod -> "rem", ""
        | Lt  -> "slt", ""
        | Gt  -> "sgt", ""
        | Le  -> "sgt", Printf.sprintf "\n\txori %s, %s, 1" left_back_reg left_back_reg
        | Ge  -> "sge", ""
        | Eq  -> "sub", Printf.sprintf "\n\tseqz %s, %s" left_back_reg left_back_reg
        | Neq -> "sub", Printf.sprintf "\n\tsnez %s, %s" left_back_reg left_back_reg
        | And -> "and", ""
        | Or  -> "or", ""
      in

      (* 6️⃣ 组合所有代码 *)
      let final_code =
        left_code @ left_move_code @ push_code @
        right_code @
        pop_code @
        [Printf.sprintf "%s %s, %s, %s%s"
           opstr left_back_reg left_back_reg right_reg extra]
      in

      (* 7️⃣ 返回的寄存器是 left_back_reg（已经保存了运算结果），
            后续的 `Return` 会把它搬到 a0。 *)
      (left_back_reg, final_code)

  | UnOp (Neg, e) ->
      let reg, code = gen_expr env depth e in
      (reg, code @ [Printf.sprintf "neg %s, %s" reg reg])

  | UnOp (Not, e) ->
      let reg, code = gen_expr env depth e in
      (reg, code @ [Printf.sprintf "seqz %s, %s" reg reg])

  | UnOp (Pos, e) ->
      let reg, code = gen_expr env depth e in
      (reg, code)

  (* ----------------------------------------------------------- *)
  (* Call：保存 caller‑saved 寄存器 → 调用 → 把返回值搬到 tmp *)
  (* ----------------------------------------------------------- *)
  | Call (fname, args) ->
    let extra = List.length args - max_arg_regs in
    let save_area = ((Array.length reg_tmp * word_size + 15) / 16) * 16 in
    let code = ref [] in

    (* ① 预留额外参数的栈空间（如果有的话） *)
    if extra > 0 then
      code := !code @ [Printf.sprintf "addi sp, sp, -%d" (extra * word_size)];

    (* ② 为 t0‑t6 的保存区腾出空间 *)
    code := !code @ [Printf.sprintf "addi sp, sp, -%d" save_area];
    for i = 0 to Array.length reg_tmp - 1 do
      code := !code @ [Printf.sprintf "sw %s, %d(sp)" reg_tmp.(i) (i*word_size)]
    done;

    (* ③ 生成实参并放到正确位置 *)
    List.iteri (fun i arg ->
      let reg, c = gen_expr env (depth+i+1) arg in
      code := !code @ c;
      if i < max_arg_regs then
        code := !code @ [Printf.sprintf "mv a%d, %s" i reg]
      else
        let off = (i - max_arg_regs) * word_size in
        code := !code @ [Printf.sprintf "sw %s, %d(sp)" reg off]
    ) args;

    (* ④ 调用函数 *)
    code := !code @ [Printf.sprintf "call %s" fname];

    (* ⑤ 恢复 t0‑t6 *)
    for i = 0 to Array.length reg_tmp - 1 do
      code := !code @ [Printf.sprintf "lw %s, %d(sp)" reg_tmp.(i) (i*word_size)]
    done;
    code := !code @ [Printf.sprintf "addi sp, sp, %d" save_area];

    (* ⑥ 弹回额外参数空间 *)
    if extra > 0 then
      code := !code @ [Printf.sprintf "addi sp, sp, %d" (extra * word_size)];

    (* ⑦ 把返回值搬到临时寄存器供上层使用 *)
    let tmp = tmp_reg_of_depth (depth+300) in
    code := !code @ [Printf.sprintf "mv %s, a0" tmp];
    (tmp, !code)


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
      (* ① 新建仅保存名字的子作用域 *)
      let block_env = push_block_env env in
      (* ② 生成块内部代码 *)
      let code' = List.fold_left (fun acc s -> gen_stmt block_env depth acc s) code ss in
      (* ③ 把块里使用的最深栈偏移写回父环境 *)
      if block_env.cur_offset < env.cur_offset then
        env.cur_offset <- block_env.cur_offset;
      code'
  | If (cond, s_then, Some s_else) ->
      let l_else = fresh_label "else" in
      let l_end  = fresh_label "endif" in
      let reg, ccond = gen_expr env depth cond in
      let c_then = gen_stmt env depth [] s_then in
      let c_else = gen_stmt env depth [] s_else in
      let then_returns = ends_with_return s_then in
      let else_returns = ends_with_return s_else in
      code @ ccond @
      [Printf.sprintf "beqz %s, %s" reg l_else] @
      c_then @
      (if not then_returns then [Printf.sprintf "j %s" l_end] else []) @
      [Printf.sprintf "%s:" l_else] @
      c_else @
      (* 只要 **任意** 分支不返回，就必须有结束标签 *)
      (if not (then_returns && else_returns) then [Printf.sprintf "%s:" l_end] else [])
  | If (cond, s_then, None) ->
      let l_end = fresh_label "endif" in
      let reg, ccond = gen_expr env depth cond in
      let c_then = gen_stmt env depth [] s_then in
      let then_returns = ends_with_return s_then in
      code @ ccond @
      [Printf.sprintf "beqz %s, %s" reg l_end] @
      c_then @
      (if not then_returns then [Printf.sprintf "j %s" l_end] else []) @
      [Printf.sprintf "%s:" l_end]   (* ← **始终**输出结束标签 *)
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
      code @ c @ [
        Printf.sprintf "mv a0, %s" reg;
        Printf.sprintf "j %s" env.ret_label
      ]

let gen_func (f : func) =
  (* 1️⃣ 创建环境并为形参分配栈槽（仅记录偏移） *)
  let env = push_env () in
  List.iteri (fun _ name -> ignore (alloc_var env name)) f.params;

  (* 2️⃣ 生成唯一返回标签 *)
  let ret_label = fresh_label (f.name ^ "_ret") in
  env.ret_label <- ret_label;

  (* 3️⃣ 递归生成函数体代码（顺便为局部变量再分配偏移） *)
  let body_code = List.fold_left (fun acc s -> gen_stmt env 0 acc s) [] f.body in

  (* 4️⃣ 计算栈大小：参数+局部变量+ra/fp，向上取整到 16 B *)
  let used_bytes = -env.cur_offset in
  let stack_size = ((used_bytes + 8 + 15) / 16) * 16 in

  (* 5️⃣ 序言 *)
  let prologue = [
    Printf.sprintf ".globl %s" f.name;
    Printf.sprintf "%s:" f.name;
    Printf.sprintf "addi sp, sp, -%d" stack_size;   (* 为整个栈帧预留空间 *)
    "sw ra, 0(sp)";
    "sw fp, 4(sp)";
    Printf.sprintf "addi fp, sp, %d" stack_size   (* fp 指向原 sp，即保存 ra/fp 之上的位置 *)
  ] in

  (* 6️⃣ 把形参保存到栈槽 *)
  let param_store =
  List.mapi (fun i name ->
    let ofs = Hashtbl.find env.stack_offset name in
    if i < max_arg_regs then
      (* 前 8 个寄存器参数 *)
      Printf.sprintf "sw a%d, %d(fp)" i ofs
    else
      (* 第 9、10 … 个参数已经在调用者栈上，偏移从 0(fp) 开始 *)
      let off = (i - max_arg_regs) * word_size in
      Printf.sprintf "lw t0, %d(sp)\nsw t0, %d(fp)" off ofs
  ) f.params
  in

  (* 7️⃣ 结束序列（返回标签、恢复 ra/fp、释放栈） *)
  let epilogue = [
    Printf.sprintf "%s:" ret_label;
    "lw ra, 0(sp)";
    "lw fp, 4(sp)";
    Printf.sprintf "addi sp, sp, %d" stack_size;
    "ret"
  ] in

  (* 8. 把所有片段拼接起来返回 *)
  String.concat "\n"
    (prologue @ param_store @ body_code @ epilogue)

let gen_program (prog : program) : string =
  String.concat "\n\n" (List.map gen_func prog)