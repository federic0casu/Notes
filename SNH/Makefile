EXT     = md

SRC_DIR = src
PDF_DIR = pdf

INPUT_UNIX  = $(SRC_DIR)/unix.$(EXT)
OUTPUT_UNIX = $(PDF_DIR)/unix.pdf

INPUT_STACK  = $(SRC_DIR)/stack.$(EXT)
OUTPUT_STACK = $(PDF_DIR)/stack.pdf


MARGIN_SIZE  = "1.2in"
MAINFONT     = "Palatino"
SANSFONT     = "Helvetica"
MONOFONT     = "Menlo"
FONTSIZE     = 10pt
PDF_ENGINE   = pdflatex

FLAGS = --variable margin=$(MARGIN_SIZE) --variable mainfont=$(MAINFONT) --variable sansfont=$(SANSFONT) --variable monofont=$(MONOFONT) --variable fontsize=$(FONTSIZE) --variable -pdf-engine=$(PDF_ENGINE)

PANDOC_COMMAND = pandoc

all: unix stack

unix: $(INPUT_UNIX)
	$(PANDOC_COMMAND) $(INPUT_UNIX) $(FLAGS) -o $(OUTPUT_UNIX)

stack: $(INPUT_STACK)
	$(PANDOC_COMMAND) $(INPUT_STACK) $(FLAGS) -o $(OUTPUT_STACK)

clean:
	rm -rf $(PDF_DIR)/*.pdf
