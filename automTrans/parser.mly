%token <string> LETTER
%token DINSY DSTSY DSTAT DINST DINSS TRANS PARG PARD VIRG PVIRG EOF
%start <Syntaxe.automate> automate
%{ open Syntaxe %}
%%


automate : decla=declarations trans=transitions EOF { (decla, trans) }

declarations : a=inputsymbols b=stacksymbols c=states d=initialstate e=initialstack
  { Declarations(a, b, c, d, e) }

inputsymbols : DINSY a=suitelettresnonvide { Inputsymbols((a)) }

stacksymbols : DSTSY b=suitelettresnonvide { Stacksymbols((b)) }

states : DSTAT c=suitelettresnonvide { States((c)) }

initialstate : DINST b=LETTER { Initialstate(b) }

initialstack : DINSS b=LETTER { Initialstack(b) }

suitelettresnonvide :
  | a=LETTER { a::[] }
  | a=LETTER VIRG b=suitelettresnonvide { (a::b) }

transitions : TRANS a=translist { Transitions((a)) }

translist : 
  | a=transition b=translist { (a::b) }
  | { [] }

transition : PARG a=LETTER VIRG b=lettreouvide VIRG c=LETTER VIRG d=LETTER VIRG e=stack PARD
  { Transition(a,b,c,d,e) }

lettreouvide : 
  | a=LETTER {LETTER(a)}
  | { Epsilon }

stack :
  | a=nonemptystack { Stack(Nonemptystack(a))}
  | { Epsilon }

nonemptystack :
  | a=LETTER { (a::[]) }
  | a=LETTER PVIRG b=nonemptystack { (a::b)}
