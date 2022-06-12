type nonemptystack = 
  | Nonemptystack of string list

type stack = 
  | Stack of nonemptystack
  | Epsilon

type letterouvide =
  | LETTER of string
  | Epsilon

type transition = Transition of (string * letterouvide * string * string * stack)

type transitions = Transitions of transition list

type initialstack = Initialstack of string

type initialstate = Initialstate of string

type states = States of string list

type stacksymbols = Stacksymbols of string list

type inputsymbols = Inputsymbols of string list

type declarations = Declarations of inputsymbols * stacksymbols * states * initialstate * initialstack

type automate = declarations * transitions
              