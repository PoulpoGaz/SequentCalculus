
(*type 'a formula =
| Top
| Bot
| V of 'a
| Not of 'a formula
| And of 'a formula * 'a formula
| Or of 'a formula * 'a formula
| Impl of 'a formula * 'a formula
*)
let () =
  Printexc.record_backtrace true;

  let formula2 = "((a -> (b -> c)) -> c) -> ((((a -> c) -> c) -> (((b -> c) -> c) -> c)) -> c)" in
  let tree2 = Parser.parse_formula formula2 in


  let seq1 = Formula.create_sequent [] [tree2] in

  let output = "output.tex" in
  let out = open_out output in
  Formula.latex_header out;
  
  Formula.proof_search_latex out (fun i -> i) seq1 |> Printf.printf "%B\n";

  Formula.latex_footer out;
  close_out out;

  Sys.command ("pdflatex " ^ output) |> Printf.printf "%d\n"