PNGS=$(patsubst %.tex, %.png, $(wildcard *.tex))
DEPS=hierarchy.tex.inc

all: $(PNGS)

%.png: %.tex $(DEPS)
	pdflatex $(patsubst %.png, %.tex, $@)
	convert -density 90 $(patsubst %.png, %.pdf, $@) -quality 100 $@

clean:
	rm -rf *.png
