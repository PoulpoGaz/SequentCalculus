type token = Var of string | OpenP | ClosedP | Impl | And | Or | Not | Comma | Turnstide | End

let operator_names = [("->", Impl); ("&&", And); ("||", Or); ("not", Not); (",", Comma); ("|-", Turnstide)]

exception ParseException of string

let throw error i =
  let err = match i with
    | Some(i) -> Printf.sprintf "%s (at %d)" error i
    | _ -> error
  in raise (ParseException err) 

let print_token token = match token with
  | And -> Printf.printf "&&"
  | Or -> Printf.printf "||"
  | Impl -> Printf.printf "->"
  | Not -> Printf.printf "!"
  | Var(var) -> Printf.printf "%s" var
  | ClosedP -> Printf.printf ")"
  | OpenP -> Printf.printf "("
  | Comma -> Printf.printf ","
  | Turnstide -> Printf.printf "|-"
  | End -> ()

let print_stack stack printer =
  Printf.printf "<";

  stack |> Stack.to_seq
        |> List.of_seq
        |> List.rev
        |> List.iter (fun f -> printer f; Printf.printf "; ")
  ;

  Printf.printf ">"


(* Returns the index of the first non whitespace or the length of the string *)
let skip_whitespace string start = 
  let rec aux string i length =
    if i >= length then i
    else if string.[i] = ' ' then aux string (i + 1) length
    else i
  in
  aux string start (String.length string)

(* Returns true if str1 contains str2 at the index start*)
let equals str1 str2 start =
  let rec aux str1 l1 str2 l2 i = 
    if i >= l2 then true
    else if i + start >= l1 then false
    else if str1.[i + start] = str2.[i] then aux str1 l1 str2 l2 (i + 1)
    else false 
  in 
  aux str1 (String.length str1) str2 (String.length str2) 0

let next_word string i operators =
  let is_keyword i =
    string.[i] = '(' || 
    string.[i] = ')' || 
    List.exists (fun (name, _) -> equals string name i) operators
  in

  let rec aux j len =
     if j >= len || string.[j] = ' ' || is_keyword j then String.sub string i (j - i)
    else aux (j + 1) len
  in aux i (String.length string)

let next_token string i operators =
  let n = String.length string in
  let j = skip_whitespace string i in

  if j >= n then (End, n)
  else if string.[j] = '(' then (OpenP, j + 1)
  else if string.[j] = ')' then (ClosedP, j + 1)
  else (
    let op = List.find_opt (fun (name, _) -> equals string name j) operators in

    match op with
      | Some((op_name, op)) -> (op, j + String.length op_name)
      | _ -> 
        let word = next_word string j operators in 
        (Var(word), j + String.length word)
  )
  
let is_var token = match token with
  | Var(_) -> true
  | _ -> false

let is_operator token = match token with
  | Impl | And | Or | Not -> true
  | _ -> false

let is_var_opt token = match token with
  | Some(Var(_)) -> true
  | _ -> false

let is_operator_opt token = match token with
  | Some(token) -> is_operator token
  | _ -> false

(* Assume that op1 and op2 are operators*)
let has_higher_precedence op1 op2 = match op1, op2 with
  | Not, _    -> true    (* not has priority over everything *)
  | Impl, Not -> false 
  | Impl, _   -> true    (* impl has priority over and, or and impl*)
  | And, Not  -> false
  | And, Impl -> false
  | And, _    -> true    (* and has priority over or and and*)
  | Or, Or    -> true    (* or has priority over or*)
  | _         -> false

let rec push_operator operator stack output = 
  if not (Stack.is_empty stack) then (
    let op2 = Stack.top stack in

    if is_operator op2 && has_higher_precedence op2 operator then (
      Stack.push (Stack.pop stack) output;
      push_operator operator stack output;
    ) else (
      Stack.push operator stack
    )
  ) else (
    Stack.push operator stack
  )

let rec unpack_parenthesis stack output =
  if Stack.is_empty stack then throw "Mismatch parenthesis" None
  else (
    let token = Stack.pop stack in

    if token <> OpenP then (
      Stack.push token output;
      unpack_parenthesis stack output;
    )
  )

let rec create_formula stack =
  if Stack.is_empty stack then raise (Invalid_argument "Can't create formula. Stack is empty")
  else (
    let token = Stack.pop stack in

    match token with
      | Var(a) -> Formula.V(a)
      | Not -> Formula.Not(create_formula stack)
      | And -> 
        let right = create_formula stack
        and left = create_formula stack in
        Formula.And(left, right)
      | Or -> 
        let right = create_formula stack
        and left = create_formula stack in
        Formula.Or(left, right)
      | Impl -> 
        let right = create_formula stack
        and left = create_formula stack in
        Formula.Impl(left, right)
      | _ -> raise (Invalid_argument "Can't create formula. Stack must only contains 'variables', 'not', 'and', 'or' and 'impl'")
  )

let create_formula stack output =
  if Stack.is_empty stack && Stack.is_empty output then raise (Invalid_argument "Empty formula");

  Printf.printf "Creating formula: ";
  print_stack stack print_token; print_stack output print_token; Printf.printf "\n";

  while not (Stack.is_empty stack) do
    let token = Stack.pop stack in

    if token = OpenP then throw "Invalid parenthesis" None
    else Stack.push token output
  done;

  let formula = create_formula output in
  if not (Stack.is_empty output) then throw "Ambigous input" None;
  formula

let check_binairy_op last j = match last with
  | None | Some(OpenP) | Some(Turnstide) | Some(Comma) -> 
    throw "Binairy operators must be preceded by a variable or a closed parenthesis" (Some j);
  |_ -> ()

let check_unary_op last j = match last with
  | Some(Var(_)) | Some(ClosedP) -> 
    throw "Unary operators can't be preceded by a variable or an open parentehsis" (Some j)
  | _ -> ()

let check_closed_p last j = match last with
  | Some(Var(_)) | Some(ClosedP) -> ()
  | _ -> throw "Closed parenthesis must be preceddd by a variable or a closed parenthesis" (Some j)

let parse_sequent string =
  let can_create_formula stack output = not (Stack.is_empty stack && Stack.is_empty output) in

  let rec loop len i delta gamma parsing_delta stack output last_token =
    let (token, j) = next_token string i operator_names in

    Printf.printf "%d - %d\n" j len;
    print_stack stack print_token; Printf.printf " - ";
    print_stack output print_token; print_newline ();

    match token with
      | Var(_) -> 
        if is_var_opt last_token then throw "Two consecutive variables" (Some(j));

        Stack.push token output; 
        loop len j delta gamma parsing_delta stack output (Some(token))

      | Impl | And | Or ->
        check_binairy_op last_token j;
        push_operator token stack output; 
        loop len j delta gamma parsing_delta stack output (Some(token))
      | Not ->
        check_unary_op last_token j;
        push_operator token stack output; 
        loop len j delta gamma parsing_delta stack output (Some(token))
      | OpenP -> 
        if is_var_opt last_token then throw "Open parenthesis can't be preceded by a variable" (Some(j));

        Stack.push token stack; 
        loop len j delta gamma parsing_delta stack output (Some(token))

      | ClosedP -> 
        check_closed_p last_token j;
        unpack_parenthesis stack output; 
        loop len j delta gamma parsing_delta stack output (Some(token))

      | Comma ->
        if can_create_formula stack output then (
          let formula = create_formula stack output in

          if parsing_delta then loop len j (formula::delta) gamma parsing_delta stack output (Some(token))
          else loop len j delta (formula::gamma) parsing_delta stack output (Some(token))

        ) else (
          throw "Empty formula" (Some(j))
        )

      | Turnstide -> 
        if parsing_delta then (
          if can_create_formula stack output then (
            let formula = create_formula stack output in

            loop len j (formula::delta) gamma false stack output (Some(token));
          ) else (
            loop len j delta gamma false stack output (Some(token));
          )
        ) else throw "Multiple turnstide" (Some(j))

      | End -> 
        if parsing_delta then throw "No turnstide" (Some(j))
        else if can_create_formula stack output then (
          let formula = create_formula stack output in
          Formula.create_sequent delta (formula::gamma)
        ) else (
          Formula.create_sequent delta gamma
        )
  in loop (String.length string) 0 [] [] true (Stack.create ()) (Stack.create ()) None