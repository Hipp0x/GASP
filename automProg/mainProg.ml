  open InterpProg
  
  let ch = open_in (Sys.argv.(1)) 
  let lexbuf = Lexing.from_channel ch 
  let ast = ParserProg.automate LexerProg.lexeur lexbuf

  let _ = InterpProg.autom ast (Sys.argv.(2)) ;;