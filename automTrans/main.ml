  open Interp
  
  let ch = open_in (Sys.argv.(1)) 
  let lexbuf = Lexing.from_channel ch 
  let ast = Parser.automate Lexer.lexeur lexbuf

  let _ = Interp.autom ast (Sys.argv.(2)) ;;
  