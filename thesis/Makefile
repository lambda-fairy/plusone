thesis.pdf: thesis.tex base.tex
	latexmk -pdf -halt-on-error $<

.PHONY: clean
clean:
	rm *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.pdf
