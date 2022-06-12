open SyntaxeProg

let rec print_liste l =
  match l with
  | [] -> print_string ".\n"
  | a::b -> print_string (a^",") ; print_liste b

let rec get_case value cases =
  match cases with
  | Cases(caselist) -> 
      match caselist with
      | [] -> None
      |(case::caselist) ->
          let newCases = Cases(caselist) in
          match case with
          |Case(str, dist) -> 
            if(String.equal value str) then
              Some case
            else
              get_case value newCases
          |FinalCase(str, act) ->
              if(String.equal value str) then
                Some case
              else
              get_case value newCases

let rec print_liste_conf l =
  match l with
  | [] -> print_string "";
  | a::b -> print_string (a) ; print_liste_conf b              

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


let rec get_action etat stack lettre progr  =
  match progr with
  | ProgAct(act) -> Some act
  | ProgDis(dist) -> 
      match dist with
      | Distinct(dinst, cases) -> 
          match dinst with
          | "top" ->(match get_case stack cases with
                    | None ->  None
                    | Some FinalCase(value, action) -> Some action
                    | Some Case(value, distinct) -> get_action etat stack lettre (ProgDis(distinct)))
          | "next" -> (match get_case lettre cases with
                      | None ->  None
                      | Some FinalCase(value, action) -> Some action
                      | Some Case(value, distinct) -> get_action etat stack lettre (ProgDis(distinct)))
          | "state" -> (match get_case etat cases with
                        | None ->  print_string "None\n";None
                        | Some FinalCase(value, action) -> Some action
                        | Some Case(value, distinct) -> get_action etat stack lettre (ProgDis(distinct)))
          | _ -> print_string "None";None
                      

let interprete st stk tr1 mt = 

  let rec action m state stack progr configs = 
    if (List.length stack != 0) then 
      (
        if List.length m = 0 then
          (
            let letter = "" in
            let pile = List.hd stack in
            let act = get_action state pile letter progr in
            match act with
            | None -> (print_string "Mot vide et pas de cas applicable.\nListe stack : \n";print_config configs)
            | Some Pop -> 
              (
                let mm = [] in
                match stack with
                |x::rest ->
                  action mm state rest progr ( (state,rest,mm)::configs )
                | [] -> print_string "Impossible"
              )
            | Some Push(symb) ->
              (
                let stck = symb::stack in
                action [] state stck progr ( (state,stck,[])::configs ) 
              )
            | Some Reject ->(print_string "Ce mot n'est pas accepté par cet automate!\n"; print_liste stack)
            | Some Change(etat) -> let newState = etat in action [] newState stack progr ( (state,stack,[])::configs )
          )
        else 
          (
            let letter = List.hd m in
            let pile = List.hd stack in
            let act = get_action state pile letter progr in
            let mm = List.tl m in
            match act with
            | None -> (print_string "Pas de cas applicable.\nListe stack : \n";print_config configs )
            | Some Pop -> 
              (
                match stack with
                |x::rest ->
                  action mm state rest progr ( (state,rest,mm)::configs )
                | [] -> print_string "Impossible"
              )
            | Some Push(symb) ->
              (
                let stck = symb::stack in
                action mm state stck progr ( (state,stck,mm)::configs ) 
              )
            | Some Reject ->(print_string "Ce mot n'est pas accepté par cet automate!\n"; print_liste stack)
            | Some Change(etat) -> let newState = etat in action mm newState stack progr ( (newState,stack,mm)::configs )
          )
      )
    else
      (
        if List.length m = 0 then
          print_config (List.rev configs)
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

let rec sameCase cases used =
match cases with
|Cases(caselist) -> 
  match caselist with
  | [] -> false
  | case::rest -> 
    match case with
    |FinalCase(str, act) -> 
      if(List.exists (fun x -> x = str) used) then
        true                  
      else
        sameCase (Cases(rest)) (str::used)

    |Case(str, distin) ->
      if(List.exists (fun x -> x = str) used) then
        true                  
      else
        sameCase (Cases(rest)) (str::used)

(*Pour le cas Dist vérifie dans chaque liste de cas si *)  
let rec deterministe prog =
  match prog with
  |  ProgAct(act) -> true
  |  ProgDis(dist) -> 
      match dist with
      |Distinct(distinc, cases) -> not (sameCase cases [])
 
let verification stacksymbols states initstate initstack progr =
  if (List.exists (fun x -> x = initstate) states) then (
    if (List.exists (fun x -> x = initstack) stacksymbols) then (
      if (deterministe progr) then (
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

  let autom (dcl,progr) mot = 
  match dcl with
  | Declarations(a,b,c,d,e) -> 
    let symbstck = getStackSymb b in
    let states = getStates c in
    let stck = getStack e in
    let ste = getState d in
    if (verification symbstck states  ste stck progr) then
      interprete ste (stck::[]) progr mot