let () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let prog = Parser.main Lexer.read lexbuf in
  close_in ic;
  Semantic.analyze prog;
  let asm = Riscv_codegen.gen_program prog in
  print_endline asm