EXT     = md

SRC_DIR = src
PDF_DIR = pdf

INPUT     = $(SRC_DIR)/source.$(EXT)
OUTPUT    = $(PDF_DIR)/ecs.pdf

MARGIN_SIZE     = "1.2in"
MAINFONT        = "Palatino"
SANSFONT        = "Helvetica"
MONOFONT        = "Menlo"
FONTSIZE        = 10pt
PDF_ENGINE      = pdflatex

FLAGS      = --variable margin=$(MARGIN_SIZE) --variable mainfont=$(MAINFONT) --variable sansfont=$(SANSFONT) --variable monofont=$(MONOFONT) --variable fontsize=$(FONTSIZE) --variable -pdf-engine=$(PDF_ENGINE)

PANDOC_COMMAND = pandoc

all: $(SRC_DIR)/source.md
	$(PANDOC_COMMAND) $(INPUT) $(FLAGS) -o $(OUTPUT)

clean:
	rm -rf $(PDF_DIR)/*.pdf
