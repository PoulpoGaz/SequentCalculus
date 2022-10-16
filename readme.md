# [Sequent Calculus](https://en.wikipedia.org/wiki/Sequent_calculus) [![License](https://img.shields.io/badge/license-MIT-red)](LICENSE)

An OCaml program that prooves sequents.
Just use: `dune exec SequentCalculus "F1, ..., FN |- G1, ... GN"` where F1, ..., GN are formulas.

## Examples

![](images/image1.png)

Output of: `dune exec SequentCalculus "|- p || not p"`

![](images/image2.png)

Output of: `dune exec SequentCalculus "|- \\phi -> ((\\phi -> \\varphi) -> \\varphi)"`

![](images/image3.png)

Output of: `dune exec SequentCalculus "|- ((a -> (b -> c)) -> c) -> ((((a -> c) -> c) -> (((b -> c) -> c) -> c)) -> c)"`

![](images/image4.png)

Output of: `dune exec SequentCalculus "|- (((\\land && \\land) || (\\lor || \\lor)) -> (not (not \\neg))) -> ((((not \\rightarrow) -> \\rightarrow) -> (\\land && \\land)) -> (((not \\rightarrow) -> \\rightarrow) -> (not (not \\neg))))"`