let () =
  (* 读取所有标准输入内容，直到 EOF *)
  let source =
    let buf = Buffer.create 4096 in
    (try
      while true do
        Buffer.add_string buf (input_line stdin);
        Buffer.add_char buf '\n'
      done
    with End_of_file -> ());
    Buffer.contents buf
  in

  try
    (* 创建词法缓冲区 *)
    let lexbuf = Lexing.from_string source in

    (* 语法分析，得到 AST *)
    let program =
      try
        Parser.main Lexer.read lexbuf
      with e ->
        prerr_endline "Parse error!";
        raise e
    in

    (* 语义分析 *)
    Semantic.analyze program;

    (* 代码生成（输出 RISC-V 汇编）*)
    let asm = Riscv_codegen.gen_program program in

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