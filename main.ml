open Ast

let () =
  (* 读取 stdin 的所有内容 *)
  let source =
    let buf = Buffer.create 4096 in
    try
      while true do
        Buffer.add_string buf (input_line stdin);
        Buffer.add_char buf '\n'
      done;
      Buffer.contents buf
    with End_of_file ->
      Buffer.contents buf
  in

  try
    (* 创建词法缓冲区 *)
    let lexbuf = Lexing.from_string source in

    (* 语法分析（假设 parse_program 实现了） *)
    let program =
      try
        Parse.parse_program Lex.token lexbuf
      with e ->
        prerr_endline "Parse error!";
        raise e
    in

    (* 语义分析 *)
    analyze program;

    (* 代码生成 *)
    let asm = gen_program program in

    (* 输出到标准输出 *)
    print_string asm;
    flush stdout

  with
  | Failure msg | Invalid_argument msg ->
      prerr_endline ("Error: " ^ msg);
      exit 1
  | e ->
      prerr_endline ("Unknown error: " ^ Printexc.to_string e);
      exit 2