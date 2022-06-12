%token <string> LETTER TOP NEXT STATE
%token DINSY DSTSY DSTAT DINST DINSS VIRG EOF
%token PROGR DPOIN BEGIN END APUSH ACPOP CASE OF AREJE ACHAN 
%start <SyntaxeProg.automate> automate
%{ open SyntaxeProg %}
%%


automate : decla=declarations progr=program EOF { print_string "automate\n";(decla, progr) }

declarations : a=inputsymbols b=stacksymbols c=states d=initialstate e=initialstack
  { print_string "declarations\n";Declarations(a, b, c, d, e) }

inputsymbols : DINSY a=suitelettresnonvide { print_string "inputsymbols\n";Inputsymbols(a) }

stacksymbols : DSTSY b=suitelettresnonvide { print_string "stacksymbols\n";Stacksymbols(b) }

states : DSTAT c=suitelettresnonvide { print_string "states\n";States((c)) }

initialstate : DINST b=LETTER { print_string "initialstate\n";Initialstate(b) }

initialstack : DINSS b=LETTER { print_string "initialstack\n";Initialstack(b) }

suitelettresnonvide :
  | a=LETTER { print_string "suitelettrenonvide\n";(a::[]) }
  | a=LETTER VIRG b=suitelettresnonvide { print_string "suitelettresnonvide\n";(a::b) }

action : 
    | APUSH b=LETTER { print_string "action\n";Push(b) }
    | ACPOP            { print_string "action\n";Pop }
    | AREJE            { print_string "action\n";Reject }
    | ACHAN b=LETTER { print_string "action\n";Change(b) }

case :  
    | a=LETTER DPOIN b=action { print_string "case\n";FinalCase(a,b) }        
    | a=LETTER DPOIN BEGIN b=distinct END { print_string "case\n";Case(a,b) }

caselist : 
    | a=case b=caselist {print_string "caselist\n"; (a::b) }
    | { [] }

cases : 
    | a=caselist { print_string "cases\n";Cases(a) }

distinct :
    | CASE a=TOP OF b=cases   { print_string "distinct\n";Distinct(a,b) }
    | CASE a=NEXT OF b=cases  {print_string "distinct\n"; Distinct(a,b) }
    | CASE a=STATE OF b=cases  { print_string "distinct\n";Distinct(a,b) }


program : 
    | PROGR a=distinct { print_string "program\n";ProgDis(a) }
    | PROGR a=action {print_string "program\n";ProgAct(a)}

