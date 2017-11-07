RNWFILES = $(wildcard *.rnw)
TEX = $(patsubst %.rnw,%.tex,$(RNWFILES)) #$(wildcard *.tex)
TARGET = WQreport
LATEX_ARGS=--interaction=nonstopmode --output-driv="xdvipdfmx -vv -V 4"
PDFLATEX_ARGS=--interaction=nonstopmode
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
