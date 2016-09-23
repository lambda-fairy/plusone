presentation.pdf: presentation.tex base.tex
	latexmk -halt-on-error -pdf $<

.PHONY: clean
clean:
	rm -f *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.pdf
