
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
  Parser.parse_formula "((a -> b) && c) || ((d && e) || f)";
  print_newline ();