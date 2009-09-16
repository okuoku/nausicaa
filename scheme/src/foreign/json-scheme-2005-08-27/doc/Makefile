all: packrat.pdf

%.pdf: %.lyx
	lyx -e pdf $<

%.html: %.lyx
	lyx -e html $<

clean:
	rm -f packrat.pdf
	rm -f packrat.html packrat2.html
