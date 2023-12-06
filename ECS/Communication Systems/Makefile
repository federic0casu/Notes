EXT     = md

SRC_DIR = src
PDF_DIR = pdf

INPUT_RADIO      = $(SRC_DIR)/radio.$(EXT)
OUTPUT_RADIO     = $(PDF_DIR)/radio.pdf

INPUT_CHANNEL    = $(SRC_DIR)/channel.$(EXT)
OUTPUT_CHANNEL   = $(PDF_DIR)/channel.pdf

INPUT_MULTI      = $(SRC_DIR)/multi.$(EXT)
OUTPUT_MULTI     = $(PDF_DIR)/multi.pdf

INPUT_DIVERSITY  = $(SRC_DIR)/diversity.$(EXT)
OUTPUT_DIVERSITY = $(PDF_DIR)/diversity.pdf

INPUT_QUEST      = $(SRC_DIR)/quest.$(EXT)
OUTPUT_QUEST     = $(PDF_DIR)/quest.pdf

MARGIN_SIZE      = "1.2in"
MAINFONT         = "Palatino"
SANSFONT         = "Helvetica"
MONOFONT         = "Menlo"
FONTSIZE         = 10pt
PDF_ENGINE       = pdflatex

FLAGS = --variable margin=$(MARGIN_SIZE) --variable mainfont=$(MAINFONT) --variable sansfont=$(SANSFONT) --variable monofont=$(MONOFONT) --variable fontsize=$(FONTSIZE) --variable -pdf-engine=$(PDF_ENGINE)

PANDOC_COMMAND = pandoc

all: radio channel multi quest diversity

radio: $(INPUT_RADIO)
	$(PANDOC_COMMAND) $(INPUT_RADIO)   $(FLAGS) -o $(OUTPUT_RADIO)

channel: $(INPUT_CHANNEL)
	$(PANDOC_COMMAND) $(INPUT_CHANNEL) $(FLAGS) -o $(OUTPUT_CHANNEL)

multi: $(INPUT_MULTI)
	$(PANDOC_COMMAND) $(INPUT_MULTI)   $(FLAGS) -o $(OUTPUT_MULTI)

diversity: $(INPUT_DIVERSITY)
	$(PANDOC_COMMAND) $(INPUT_DIVERSITY)   $(FLAGS) -o $(OUTPUT_DIVERSITY)

quest: $(INPUT_QUEST)
	$(PANDOC_COMMAND) $(INPUT_QUEST)   $(FLAGS) -o $(OUTPUT_QUEST)

clean:
	rm -rf $(PDF_DIR)/*.pdf
