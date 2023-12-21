EXT     = md

SRC_DIR = src
PDF_DIR = pdf

INPUT_PUB_KEY  = $(SRC_DIR)/pub_key.$(EXT)
OUTPUT_PUB_KEY = $(PDF_DIR)/pub_key.pdf

MARGIN_SIZE    = "1.2in"
MAINFONT       = "Palatino"
SANSFONT       = "Helvetica"
MONOFONT       = "Menlo"
FONTSIZE       = 10pt
PDF_ENGINE     = pdflatex

FLAGS = --variable margin=$(MARGIN_SIZE) --variable mainfont=$(MAINFONT) --variable sansfont=$(SANSFONT) --variable monofont=$(MONOFONT) --variable fontsize=$(FONTSIZE) --variable -pdf-engine=$(PDF_ENGINE)

PANDOC_COMMAND = pandoc

all: pubkey

pubkey: $(INPUT_PUB_KEY)
	$(PANDOC_COMMAND) $(INPUT_PUB_KEY) $(FLAGS) -o $(OUTPUT_PUB_KEY)

clean:
	rm -rf $(PDF_DIR)/*.pdf
