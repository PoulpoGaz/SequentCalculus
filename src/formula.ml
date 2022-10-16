type 'a formula =
  | Top
  | Bot
  | V of 'a
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Impl of 'a formula * 'a formula

type 'a sequent = {
  gamma : 'a formula list ;
  delta : 'a formula list ;
  gamma_var : 'a formula list ;
  delta_var : 'a formula list
}

exception Wrong_rule of string

let create_sequent gamma delta = {
  gamma = gamma;
  delta = delta;
  gamma_var = [];
  delta_var = []
}

let rec member x l = match l with
  | [] -> false
  | h::_ when x = h -> true
  | _::t -> member x t

let bot sequent = member Bot sequent.gamma_var || List.exists (function p -> p = Bot) sequent.gamma

let top sequent = member Top sequent.delta_var || List.exists (function p -> p = Bot) sequent.delta

let axiom sequent = 
  let rec aux gamma gamma_var = match gamma, gamma_var with
    | h::_, _ when member h sequent.delta || member h sequent.delta_var -> true
    | _, h::_ when member h sequent.delta || member h sequent.delta_var -> true
    | _::t, _ -> aux t gamma_var
    | _, _::t -> aux gamma t
    | _, _ -> false
  in aux sequent.gamma sequent.gamma_var

let and_gamma sequent = match sequent.gamma with
  | And(phi, psi)::gamma' -> {
      gamma = phi :: psi :: gamma';
      gamma_var = sequent.gamma_var;
      delta = sequent.delta;
      delta_var = sequent.delta_var
    }
  | _ -> raise (Wrong_rule "And Gamma")

let or_gamma sequent  = match sequent.gamma with
  | Or(phi, psi)::gamma' -> 
    let left = {
      gamma = phi :: gamma';
      gamma_var = sequent.gamma_var;
      delta = sequent.delta;
      delta_var = sequent.delta_var
    }
    and right = {
      gamma =  psi :: gamma';
      gamma_var = sequent.gamma_var;
      delta = sequent.delta;
      delta_var = sequent.delta_var
    }
    in (left, right)
  | _ -> raise (Wrong_rule "Or Gamma")

let impl_gamma sequent  = match sequent.gamma with
  | Impl(phi, psi)::gamma' -> 
    let left = {
      gamma = gamma';
      gamma_var = sequent.gamma_var;
      delta = phi :: sequent.delta;
      delta_var = sequent.delta_var
    }
    and right = {
      gamma = psi :: gamma';
      gamma_var = sequent.gamma_var;
      delta = sequent.delta;
      delta_var = sequent.delta_var
    }
    in (left, right)
  | _ -> raise (Wrong_rule "Impl Gamma")

let not_gamma sequent = match sequent.gamma with
  | Not(phi)::gamma' -> {
      gamma = gamma';
      gamma_var = sequent.gamma_var;
      delta = phi :: sequent.delta;
      delta_var = sequent.delta_var
    }
  | _ -> raise (Wrong_rule "Not Gamma")

  
let and_delta sequent  = match sequent.delta with
  | And(phi, psi)::delta' -> 
    let left = {
      gamma = sequent.gamma;
      gamma_var = sequent.gamma_var;
      delta = phi :: delta';
      delta_var = sequent.delta_var
    }
    and right = {
      gamma = sequent.gamma;
      gamma_var = sequent.gamma_var;
      delta = psi :: delta';
      delta_var = sequent.delta_var
    }
    in (left, right)
  | _ -> raise (Wrong_rule "And delta")


let or_delta sequent  = match sequent.delta with
  | Or(phi, psi)::delta' -> {
      gamma = sequent.gamma;
      gamma_var = sequent.gamma_var;
      delta = phi :: psi :: delta';
      delta_var = sequent.delta_var
    }
  | _ -> raise (Wrong_rule "Or Delta")


let impl_delta sequent  = match sequent.delta with
  | Impl(phi, psi)::delta' -> {
      gamma = phi :: sequent.gamma;
      gamma_var = sequent.gamma_var;
      delta = psi :: delta';
      delta_var = sequent.delta_var
    }
  | _ -> raise (Wrong_rule "Impl Delta")

let not_delta sequent  = match sequent.delta with
  | Not(phi)::delta' -> {
      gamma = phi :: sequent.gamma;
      gamma_var = sequent.gamma_var;
      delta = delta';
      delta_var = sequent.delta_var
    }
  | _ -> raise (Wrong_rule "Or Delta")



let is_simple prop = match prop with
  | Top | Bot | V(_) -> true
  | _ -> false

let rec proof_search sequent = match sequent.gamma, sequent.delta with
  | _ when axiom sequent || top sequent || bot sequent -> true

  (* GAMMA *)
  | phi :: gamma', _ when is_simple phi -> proof_search {
      gamma = gamma';
      gamma_var = phi::sequent.gamma_var;
      delta = sequent.delta;
      delta_var = sequent.delta_var;
    }
  | And(_) :: _, _ -> proof_search (and_gamma sequent)
  | Or(_) :: _, _ -> let a, b = or_gamma sequent in proof_search a && proof_search b
  | Impl(_) :: _, _ -> let a, b = impl_gamma sequent in proof_search a && proof_search b
  | Not(_) :: _, _ -> proof_search (not_gamma sequent)

  (* DELTA*)
  | _, phi :: delta' when is_simple phi -> proof_search {
      gamma = sequent.gamma;
      gamma_var = sequent.gamma_var;
      delta = delta';
      delta_var = phi :: sequent.delta_var;
    }
  | _, And(_) :: _ -> let a, b = and_delta sequent in proof_search a && proof_search b
  | _, Or(_) :: _ -> proof_search (or_delta sequent)
  | _, Impl(_) :: _ -> proof_search (impl_delta sequent)
  | _, Not(_) :: _ -> proof_search (not_delta sequent)
  | _ -> false (* gamma, delta vide et axiom, top et bot ne peuvent s'appliquer*)


(* LATEX *) 

type operator = AND | OR | NOT | IMPL 

let get_operator formula = match formula with
  | Not(_) -> NOT
  | And(_) -> AND
  | Or(_) -> OR
  | Impl(_) -> IMPL
  | _ -> failwith "variable and not formula"

let prop_to_latex out to_string prop = 
  
  let rec aux prop = match prop with
    | Top -> output_string out " \\top "
    | Bot -> output_string out " \\bot "
    | V(a) -> output_string out (to_string a)

    | Not(phi) when is_simple phi -> 
      output_string out " \\neg "; 
      aux phi

    | Not(phi) -> 
      output_string out " \\neg ("; 
      aux phi; 
      output_string out ")";

    | And(phi, psi) -> 
      append_op AND phi false;
      output_string out " \\land ";
      append_op AND psi false

    | Or(phi, psi) -> 
      append_op OR phi false;
      output_string out " \\lor "; 
      append_op OR psi false

    | Impl(phi, psi) -> 
      append_op IMPL phi true;
      output_string out " \\rightarrow "; 
      append_op IMPL psi true

  and append_op op formula forceParenthesis = match op, formula with
      | _, f when is_simple f -> aux f
      | op, formula when forceParenthesis || op <> (get_operator formula) -> (
        output_string out "("; aux formula; output_string out ")"
      )
      | _, _ -> aux formula
  in aux prop


let sequent_to_latex out to_string sequent = 
  let rec aux formulas variables = match formulas, variables with 
    | [], [] -> ()
    | h::[], [] -> prop_to_latex out to_string h
    | h::[], _ -> prop_to_latex out to_string h; output_string out ","; aux [] variables
    | [], h::[] -> prop_to_latex out to_string h
    | [], h::t -> prop_to_latex out to_string h; output_string out ","; aux [] t
    | h::t, _ -> prop_to_latex out to_string h; output_string out ","; aux t variables
  in
  aux sequent.gamma sequent.gamma_var; (* grrr *)
  output_string out " \\vdash ";
  aux sequent.delta sequent.delta_var


let output_infer out to_string sequent (n_child : int) rule_name =
  output_string out "\\infer"; output_string out (string_of_int n_child);

  (match rule_name with
    | Some(a) -> output_string out "[$("; output_string out a; output_string out ")$]"
    | _ -> ());

  output_string out "{";
  sequent_to_latex out to_string sequent;
  output_string out "}\n"

let output_hypo out to_string sequent =
  output_string out "\\hypo{";
  sequent_to_latex out to_string sequent;
  output_string out "}\n"

let proof_search_latex out to_string sequent = 
  let rec aux sequent = match sequent.gamma, sequent.delta with
    | _ when axiom sequent -> 
      output_infer out to_string sequent 0 (Option.some "Ax");
      true
    | _ when top sequent -> 
      output_infer out to_string sequent 0 (Option.some "\\top");
      true
    | _ when bot sequent -> 
      output_infer out to_string sequent 0 (Option.some "\\bot");
      true

    (* GAMMA *)
    | phi :: gamma', _ when is_simple phi -> aux {
        gamma = gamma';
        gamma_var = phi::sequent.gamma_var;
        delta = sequent.delta;
        delta_var = sequent.delta_var;
      }
    | And(_) :: _, _ -> 
      let ret = aux (and_gamma sequent) in
      output_infer out to_string sequent 1 (Option.some "\\land\\vdash");
      ret
    | Or(_) :: _, _ -> 
      let a, b = or_gamma sequent in
      let retA, retB = aux a, aux b in
      output_infer out to_string sequent 2 (Option.some "\\lor\\vdash");
      retA && retB
    | Impl(_) :: _, _ -> 
      let a, b = impl_gamma sequent in 
      let retA, retB = aux a, aux b in
      output_infer out to_string sequent 2 (Option.some "\\rightarrow\\vdash");
      retA && retB
    | Not(_) :: _, _ -> 
      let ret = aux (not_gamma sequent) in
      output_infer out to_string sequent 1 (Option.some "\\neg\\vdash");
      ret

    (* DELTA*)
    | _, phi :: delta' when is_simple phi -> aux {
        gamma = sequent.gamma;
        gamma_var = sequent.gamma_var;
        delta = delta';
        delta_var = phi :: sequent.delta_var;
      }
    | _, And(_) :: _ -> 
      let a, b = and_delta sequent in 
      let retA, retB = aux a, aux b in
      output_infer out to_string sequent 2 (Option.some "\\vdash\\land");
      retA && retB
    | _, Or(_) :: _ -> 
      let ret = aux (or_delta sequent) in
      output_infer out to_string sequent 1 (Option.some "\\vdash\\lor");
      ret
    | _, Impl(_) :: _ -> 
      let ret = aux (impl_delta sequent) in
      output_infer out to_string sequent 1 (Option.some "\\vdash\\rightarrow");
      ret
    | _, Not(_) :: _ -> 
      let ret = aux (not_delta sequent) in
      output_infer out to_string sequent 1 (Option.some "\\vdash\\neg");
      ret
    | _ -> 
      output_hypo out to_string sequent; 
      false (* gamma, delta vide et axiom, top et bot ne peuvent s'appliquer*)
  in
  output_string out "\\begin{prooftree}\n";
  let valid = aux sequent in
  output_string out "\\end{prooftree}\n";
  output_string out "\n\nValid ? "; output_string out (string_of_bool valid);
  output_string out "\n\n";

  valid

let latex_header out =
  output_string out "\\documentclass[margin=0.1cm,varwidth=500cm]{standalone}\n";
  output_string out "\\usepackage{ebproof}\n";
  output_string out "\\usepackage{amssymb}\n";
  output_string out "\n";
  output_string out "\\begin{document}\n"


let latex_footer out = 
  output_string out "\\end{document}\n"