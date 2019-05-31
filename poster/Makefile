TITLE="2019_jsm"

.PHONY : all
all: poster clean

poster:
	pdflatex --enable-write18 $(TITLE).tex
	bibtex $(TITLE)
	pdflatex $(TITLE).tex
	pdflatex $(TITLE).tex

clean:
	rm -f $(addprefix $(TITLE),.aux .bbl .blg .fff .log .mw .out)

web:
	rsync --chmod=go+r $(TITLE).pdf \
		nhejazi@arwen.berkeley.edu:/mirror/data/pub/users/nhejazi/posters/$(TITLE).pdf
