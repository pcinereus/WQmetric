RNWFILES = $(wildcard *.rnw)
TEX = $(patsubst %.rnw,%.tex,$(RNWFILES)) #$(wildcard *.tex)
TARGET = WQreport
LATEX_ARGS=--interaction=nonstopmode --output-driv="xdvipdfmx -vv -V 5"
LATEX=xelatex
RERUNBIB = "No file.*\.bbl|Citation.*undefined"
RERUNINDEX = "No file.*\.ind"

PDFS=$(wildcard figures/*.pdf)
JPGS=$(patsubst %.pdf, %.jpg, $(PDFS))
EPSS=$(patsubst %.pdf, %.eps, $(PDFS))
CONVERT = convert -density 300 -resize 33% -background white -flatten

MASTER = WQreport.pdf
DEPENDS = $(TARGET).tex

$(MASTER): $(DEPENDS)


$(TARGET).pdf: WQreport.tex
	$(LATEX) $(LATEX_ARGS) $(TARGET).tex;
	while \egrep -c $(RERUNBIB) $(TARGET).log; \
	do \
		bibtex $(TARGET); \
		$(LATEX) $(LATEX_ARGS) $(TARGET).tex; \
	done
	while \grep -q "Rerun to get cross-references right." \
	$(TARGET).log; \
	do \
		$(LATEX) $(LATEX_ARGS) $(TARGET).tex; \
	done
	$(LATEX) $(LATEX_ARGS) $(TARGET).tex;

shrink:
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile=$(TARGET)_small.pdf $(TARGET).pdf
	#gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -sOutputFile=$(TARGET)_small.pdf $(TARGET).pdf
	#gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dNOPAUSE -dQUIET -dBATCH -sOutputFile=$(TARGET)_small.pdf $(TARGET).pdf
	#gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dNOPAUSE -dQUIET -dBATCH -sOutputFile=$(TARGET)_small.pdf $(TARGET).pdf

restrict:
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -dFirstPage=1 -dLastPage=59 -sOutputFile=$(TARGET)_part1.pdf $(TARGET).pdf
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -dFirstPage=60 -dLastPage=105 -sOutputFile=$(TARGET)_part2.pdf $(TARGET).pdf
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -dFirstPage=106 -dLastPage=139 -sOutputFile=$(TARGET)_part3.pdf $(TARGET).pdf
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -dFirstPage=158 -dLastPage=160 -sOutputFile=$(TARGET)_part4.pdf $(TARGET).pdf

figs: $(JPGS) $(EPSS)

%.jpg: %.pdf
	@echo ** Building jpg images from pdf versions**
	$(CONVERT) $< $@

%.eps: %.pdf
	pdf2ps -dLanguageLevel=3 $< $@

figures::
	for image_file in $(wildcard figures/*.pdf); \
	do \
		convert -density 300 -quality 100 $${image_file} $${image_file}.jpg; \
	done;

	for image_file in $(wildcard figures/*.pdf); \
	do \
		convert -density 300 -quality 100 $${image_file} $${image_file}.tiff; \
	done;

	for image_file in $(wildcard figures/*.pdf); \
	do \
		pdftops -eps -level3 $${image_file}; \
	done;

zip:
	zip $(TARGET).zip $(TARGET).pdf *.R figures/*.pdf figures/*.jpg figures/*.eps 
clean:
	rm *.toc *.aux *.pdf *.ps *.eps *.log *.lof *.bib *.bbl *.blg *.dvi *.tex *.map
