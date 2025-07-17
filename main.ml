let () =
  try
    (* 从标准输入读取全部内容 *)
    let source_code = really_input_string stdin (in_channel_length stdin) in
    
    (* 词法分析 *)
    let lexbuf = Lexing.from_string source_code in
    (* 直接使用lexbuf进行语法分析 *)
    let ast = Parser.main Lexer.read lexbuf in
    
    (* 语义分析 *)
    Semantic.analyze ast;
    
    (* 代码生成 *)
    let asm_code = Riscv_codegen.gen_program ast in
    
    (* 输出汇编代码 *)
    print_endline asm_code
  with
  | Parser.Error ->
      prerr_endline "Syntax error";
      exit 1
  | Failure msg ->
      prerr_endline ("Error: " ^ msg);
      exit 1
  | e ->
      prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
      exit 1