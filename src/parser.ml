type token = Var of string | OpenP | ClosedP | Impl | And | Or | Not

let operator_names = [("->", Impl); ("&&", And); ("||", Or); ("!", Not)]

exception No_such_element
exception ParseException of string

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

  if j >= n then raise No_such_element;

  if string.[j] = '(' then (OpenP, j + 1)
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

let is_word_opt token = match token with
  | Some(Var(_)) -> true
  | _ -> false

let is_operator_opt token = match token with
  | Some(token) -> is_operator token
  | _ -> false

(* Assume that op1 and op2 are operators*)
let has_higher_precedence op1 op2 = match op1, op2 with
  | Impl, _   -> true   (* impl has priority over everything *)
  | And, Impl -> false
  | And, _    -> true   (* and has priority over and, or and not*)
  | Or, Impl  -> false
  | Or, And   -> false
  | Or, _     -> true   (* or has priority over or and not*)
  | Not, Not  -> true   (* not has priority over not*)
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
  if Stack.is_empty stack then raise (ParseException "Invalid parenthesis")
  else (
    let token = Stack.pop stack in

    if token <> OpenP then (
      Stack.push token output;
      unpack_parenthesis stack output;
    )
  )

let print_token token = match token with
  | And -> Printf.printf "&&"
  | Or -> Printf.printf "||"
  | Impl -> Printf.printf "->"
  | Not -> Printf.printf "!"
  | Var(var) -> Printf.printf "%s" var
  | ClosedP -> Printf.printf ")"
  | OpenP -> Printf.printf "("

let print_stack stack printer =
  Printf.printf "<";

  stack |> Stack.to_seq
        |> List.of_seq
        |> List.rev
        |> List.iter (fun f -> printer f; Printf.printf "; ")
  ;

  Printf.printf ">\n"

let rec create_tree stack =
  if Stack.is_empty stack then raise (ParseException "Empty stack")
  else (
    let token = Stack.pop stack in

    match token with
      | Var(a) -> Formula.V(a)
      | Not -> Formula.Not(create_tree stack)
      | And -> 
        let right = create_tree stack
        and left = create_tree stack in
        Formula.And(left, right)
      | Or -> 
        let right = create_tree stack
        and left = create_tree stack in
        Formula.Or(left, right)
      | Impl -> 
        let right = create_tree stack
        and left = create_tree stack in
        Formula.Impl(left, right)
      | _ -> raise (Invalid_argument "stack must only contains 'variables', 'not', 'and', 'or' and 'impl'")
  )

let parse_formula string = 
  let n = String.length string in
  let i = ref 0 in

  let stack = Stack.create () in
  let output = Stack.create () in

  while !i < n do
    let (token, j) = next_token string !i operator_names in

    Printf.printf "%d - %d\n" j n;
    print_stack stack print_token;

    (match token with
      | Var(_) -> Stack.push token output
      | Impl | And | Or | Not -> push_operator token stack output
      | OpenP -> Stack.push token stack
      | ClosedP -> unpack_parenthesis stack output
    );

    i := j
  done;

  while not (Stack.is_empty stack) do
    let token = Stack.pop stack in

    if token = OpenP then raise (ParseException "Invalid parenthesis")
    else Stack.push token output
  done;

  create_tree output