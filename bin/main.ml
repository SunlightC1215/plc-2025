
(* 读取标准输入的全部内容 *)
let read_all_input () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (read_line ());
      Buffer.add_char buf '\n'
    done;
    assert false (* 永远不会执行到这里 *)
  with End_of_file ->
    Buffer.contents buf

(* 主函数：从标准输入读取代码，编译并输出汇编 *)
let () =
  try
    (* 1. 从标准输入读取全部代码 *)
    let code = read_all_input () in
    
    (* 2. 词法分析 *)
    let lexbuf = Lexing.from_string code in
    (* 3. 语法分析 - 直接传递 lexbuf 给解析器 *)
    let ast = Parser.main Lexer.read lexbuf in
    
    (* 4. 语义分析 *)
    Semantic.analyze ast;
    
    (* 5. 代码生成 *)
    let asm = Riscv_codegen.gen_program ast in
    
    (* 6. 输出汇编代码 *)
    print_endline asm
  with
  | Parser.Error -> 
      prerr_endline "Syntax error"; exit 1
  | Failure msg -> 
      prerr_endline ("Semantic error: " ^ msg); exit 1
  | ex -> 
      prerr_endline ("Unexpected error: " ^ Printexc.to_string ex); exit 1