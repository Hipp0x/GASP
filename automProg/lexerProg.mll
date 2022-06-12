{
  open ParserProg
}

let lettre = ['0'-'9''a'-'z''A'-'Z']
let espace = [ ' ' '\t' '\n']+

rule lexeur = parse
  | "input symbols:"    { DINSY }
  | "stack symbols:"    { DSTSY }
  | "states:"           { DSTAT }
  | "initial state:"    { DINST }
  | "initial stack:"  { DINSS }
  | "program:"        { PROGR }
  | "push"            { APUSH }
  | "pop"             { ACPOP }
  | "case"            { CASE }
  | "of"              { OF }  
  | "top"             { TOP(Lexing.lexeme lexbuf) }
  | "next"            { NEXT(Lexing.lexeme lexbuf) }
  | "state"           { STATE(Lexing.lexeme lexbuf) }
  | "reject"          { AREJE }
  | "change"          { ACHAN }
  | espace            { lexeur lexbuf }
  | "begin"           { BEGIN }
  | "end"             { END }
  | ","               { VIRG }
  | ":"               { DPOIN }
  | lettre            { LETTER(Lexing.lexeme lexbuf) }
  | eof			{ EOF }
  | _ { failwith "Fichier d'entrée incorrect."}
  