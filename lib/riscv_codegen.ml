open Ast

module StringMap = Map.Make(String)
module VarEnv = Map.Make(String)
type vinfo = {
  v_ty: [`Int];
  initialized: bool;
}

type fsign = {
  ret_ty: [ `Int | `Void ];
  params: string list;
}

let global_funcs : func list ref = ref []

type env = {
  vars: (string, vinfo) Hashtbl.t;  (* 当前块的局部变量 *)
  parent: env option;
  breakable: bool;                  (* 是否在循环内部 *)
  cur_func: fsign option;           (* 当前函数签名 *)
}

let rec find_var env name =
  match Hashtbl.find_opt env.vars name with
  | Some v -> Some v
  | None ->
    (match env.parent with
    | Some p -> find_var p name
    | None -> None)

let find_func funcs name =
  try Some (List.find (fun f -> f.name = name) funcs)
  with Not_found -> None

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
      (match find_func funcs fname with
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
    let returns = analyze_stmts new_env stmts in
    (* 恢复父环境的变量状态 *)
    returns
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
  (* 参数声明 *)
  List.iter (fun name ->
    Hashtbl.add env.vars name { v_ty = `Int; initialized = true }
  ) f.params;
  let has_return = analyze_stmts env f.body in
  match f.ret_ty with
  | `Int -> if not has_return then failwith (f.name ^ " not all paths return int")
  | `Void -> ()

let analyze (prog:program) =
  global_funcs := prog;
  check_func_signatures prog;
  List.iter analyze_func prog
