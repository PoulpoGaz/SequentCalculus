let () =
  Printexc.record_backtrace true;

  if Array.length Sys.argv <= 1 then Printf.printf "No input\n"
  else (
    Printf.printf "Input: %s" Sys.argv.(1);
    let seq1 = Parser.parse_sequent Sys.argv.(1) in

    let output = "output.tex" in
    let out = open_out output in
    Formula.latex_header out;
    
    Formula.proof_search_latex out (fun i -> i) seq1 |> Printf.printf "%B\n";

    Formula.latex_footer out;
    close_out out
  )