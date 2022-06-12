open Syntaxe

let rec print_liste_conf l =
  match l with
  | [] -> print_string "";
  | a::b -> print_string (a) ; print_liste_conf b

(*affiche letat actuel sur le terminal*)
let rec print_config l =
  match l with
  | [] -> print_string "Fin.\n"
  | (x,y,z)::a -> 
    print_string ("( "^x^", ");
    print_liste_conf y;
    print_string (", ");
    print_liste_conf z;
    print_string ")\n";
    print_config a

(*retourne une transition possible, ou rien*)
let rec get_transition etat stack lettre transis =
  match transis with
  | [] -> None
  | (a,b,c,d,e)::l ->
    if (a = etat) then (
      if (b = lettre) then (
        if (c = stack) then
          Some(a,b,c,d,e)
        else 
          get_transition etat stack lettre l  
      ) else get_transition etat stack lettre l  
    ) else
      get_transition etat stack lettre l  

let rec print_liste l =
  match l with
  | [] -> print_string ".\n"
  | a::b -> print_string (a^",") ; print_liste b

(*fonction principale*)
let interprete st stk tr1 mt = 

  let rec action m state stack trans configs = 
    if (List.length stack != 0) then 
      (
        if List.length m = 0 then
          (
            let letter = "" in
            let pile = List.hd (List.rev stack) in
            let tr = get_transition state pile letter trans in
            match tr with
            | None -> (print_string "Mot vide et pas de transition applicable.\nListe stack : \n";print_liste stack )
            | Some (a,b,c,d,e) -> 
              (
                let mm = [] in
                let stck = (List.rev (List.tl (List.rev stack)))@e in
                action mm d stck trans ( (d,stck,mm)::configs )
              )
          )
        else 
          (
            let letter = List.hd m in
            let pile = List.hd (List.rev stack) in
            let tr = get_transition state pile letter trans in
            match tr with
            | None -> (print_string "Pas de transition applicable.\nListe stack : \n";print_liste stack )
            | Some (a,b,c,d,e) -> 
              (
                let mm = List.tl m in
                let stck = (List.rev (List.tl (List.rev stack)))@e in
                action mm d stck trans ( (d,stck,mm)::configs )
              )
          )
      )
    else
      (
        if List.length m = 0 then
          (print_string "Configurations : \n";
          print_config (List.rev configs)
          )
        else 
          (
            print_int (List.length m);
            print_string "Mot : \n";
            print_liste m;
            print_string "Pile vide mais mot non vide.\n"
          )
      ) 
  in

  let mot = List.map (fun x -> Char.escaped x) (List.init (String.length mt) (String.get mt)) in
  let debconf = (st,stk,mot)::[] in
  action mot st stk tr1 debconf


(* retourne la pile*)
let getStck x =
  match x with
  | Stack(Nonemptystack(a)) -> a
  | Epsilon -> []

(* retourne une lettre ou rien *)
let getLetterOuVide x =
  match x with
  | LETTER(a) -> a
  | Epsilon -> ""

(* retoune la liste des transitions *)
let rec getTransitions l trans =
  match l with
  | [] -> trans
  | Transition(a,b,c,d,e)::x -> (
    let st = getStck e in
    let b2 = getLetterOuVide b in
    getTransitions x ((a,b2,c,d,st)::trans) )

(* retourne la pile initiale *)
let getStack a = 
  match a with 
  | Initialstack(x) -> x

(* retourne l'etat initial *)
let getState a = 
  match a with 
  | Initialstate(x) -> x

(* retournes les etats *)
let getStates a =
  match a with
  | States(x) -> x

(* retourne les symboles de pile *)
let getStackSymb a =
  match a with
  | Stacksymbols(x) -> x

(* retourne les inputs symbols *)
let getInputSymb a = 
  match a with
  | Inputsymbols(x) -> x

let rec checkIfOtherNormalTrans p1 p2 p3 l nb =
  match l with
  | [] -> nb
  | (a,b,c,d,e)::k -> if ( a = p1 && b = p2 && c = p3 ) then checkIfOtherNormalTrans p1 p2 p3 k (nb + 1) else checkIfOtherNormalTrans p1 p2 p3 k nb

let rec checkIfOtherEpsTrans p1 p2 l nb =
  match l with
  | [] -> nb
  | (a,b,c,d,e)::k -> if ( a = p1 && c = p2 ) then checkIfOtherEpsTrans p1 p2 k (nb + 1) else checkIfOtherEpsTrans p1 p2 k nb

let rec deterministe tr all =
  match tr with
  | [] -> true
  | (a,b,c,d,e)::l -> 
    match b with 
    | "" -> 
      if (checkIfOtherEpsTrans a c all 0 != 1) then
        ( print_string ("epsilon : "^a^" "^b^" "^c^" "^d);
          print_int (checkIfOtherEpsTrans a c all 0);
        false )
      else 
        deterministe l all
    | x ->
      if (checkIfOtherNormalTrans a b c all 0 != 1) then
        (print_string ("normal : "^a^" "^b^" "^c^" "^d);
          print_int (checkIfOtherNormalTrans a b c all 0);
          false)
      else deterministe l all
  

let verification stacksymbols states initstate initstack trans =
  if (List.exists (fun x -> x = initstate) states) then (
    if (List.exists (fun x -> x = initstack) stacksymbols) then (
      if (deterministe trans trans) then (
        true
      ) else (
            print_string "L'automate n'est pas déterminitste.\n";
            false
      )
    ) else (
          print_string "Le symbole de pile initial n'est pas compris dans la liste des symboles de pile.\n";
          false
    )
  ) else (
    print_string "L'etat initial n'est pas compris dans la liste des etats.\n";
    false
  )


(*fonction principal d'appel*)
let autom (dcl,trs) mot = 
  match dcl with
  | Declarations(a,b,c,d,e) -> 
    match trs with 
    | Transitions(t) -> 
      let symbstck = getStackSymb b in
      let states = getStates c in
      let stck = getStack e in
      let ste = getState d in
      let tr = getTransitions t [] in
      if (verification symbstck states ste stck tr) then
        interprete ste (stck::[]) tr mot