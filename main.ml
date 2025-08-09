let () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let prog = Parser.main Lexer.read lexbuf in
    close_in ic;
    Semantic.analyze prog;
    let asm = Riscv_codegen.gen_program prog in
    print_endline asm
  with
    | Parser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Syntax error at line %d, column %d\n" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
        close_in ic
    | e -> close_in ic; raise e