type operator = Impl | And | Or | Not

let operator_names = [("->", Impl); ("&&", And); ("||", Or); ("!", Not)]

type token = Operator of operator * int | Word of string * int | Parenthesis of bool * int

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

  if string.[j] = '(' then Parenthesis(true, j + 1)
  else if string.[j] = ')' then Parenthesis(false, j + 1)
  else (
    let op = List.find_opt (fun (name, _) -> equals string name j) operators in

    match op with
      | Some((op_name, op)) -> Operator(op, j + String.length op_name)
      | _ -> 
        let word = next_word string j operators in 
        Word(word, j + String.length word)
  )
  
let is_word token = match token with
  | Word(_) -> true
  | _ -> false

let is_operator token = match token with
  | Operator(_) -> true
  | _ -> false

let is_word_opt token = match token with
  | Some(Word(_)) -> true
  | _ -> false

let is_operator_opt token = match token with
  | Some(Operator(_)) -> true
  | _ -> false

let parse_formula string = 
  let n = String.length string in
  let i = ref 0 in

  (* stack of formula or parenthesis *)
  let stack = Stack.create () in
  let last_token = ref None in

  while !i < n do
    let token = next_token string !i operator_names in

    match token with
      | Parenthesis(_, j) -> i := j
      | Word(word, j) -> i := j
      | Operator(Impl, j) -> i := j
      | Operator(And, j) -> i := j
      | Operator(Or, j) -> i := j
      | Operator(Not, j) -> i := j
    ;

    last_token := Some(token);
  done