thesis.pdf: thesis.tex base.tex sources.bib ../Cauchy.hs
	latexmk -pdf -halt-on-error -shell-escape $<

.PHONY: clean
clean:
	rm *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.pdf
