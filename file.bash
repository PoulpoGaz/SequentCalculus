#!/bin/bash

dune exec SequentCalculus "|- p || not p"
pdflatex output.tex
convert -units PixelsPerInch -density 600 output.pdf images/image1.png

dune exec SequentCalculus "|- \\phi -> ((\\phi -> \\varphi) -> \\varphi)"
pdflatex output.tex
convert -units PixelsPerInch -density 600 output.pdf images/image2.png

dune exec SequentCalculus "|- ((a -> (b -> c)) -> c) -> ((((a -> c) -> c) -> (((b -> c) -> c) -> c)) -> c)"
pdflatex output.tex
convert -units PixelsPerInch -density 600 output.pdf images/image3.png

dune exec SequentCalculus "|- (((\\land && \\land) || (\\lor || \\lor)) -> (not (not \\neg))) -> ((((not \\rightarrow) -> \\rightarrow) -> (\\land && \\land)) -> (((not \\rightarrow) -> \\rightarrow) -> (not (not \\neg))))"
pdflatex output.tex
convert -units PixelsPerInch -density 600 output.pdf images/image4.png
