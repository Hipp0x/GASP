type nonemptystack = 
  | Nonemptystack of string list

type stack = 
  | Stack of nonemptystack
  | Epsilon

type letterouvide =
  | LETTER of string
  | Epsilon

type initialstack = Initialstack of string

type initialstate = Initialstate of string

type states = States of string list

type stacksymbols = Stacksymbols of string list

type inputsymbols = Inputsymbols of string list

type declarations = Declarations of inputsymbols * stacksymbols * states * initialstate * initialstack

type action = 
  | Pop 
  | Push of string
  | Reject
  | Change of string

type case = 
  | FinalCase of (string * action)
  | Case of (string * distinct)

and cases = Cases of case list

and distinct = Distinct of string * cases

type program = 
| ProgDis of distinct
| ProgAct of action

type automate = declarations * program
              