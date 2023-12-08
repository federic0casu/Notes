INPUT  = src.md
OUTPUT = notes.pdf

CMD    = pandoc
FLAGS  = -N --variable "geometry=margin=1.1in" --variable mainfont="Palatino" --variable sansfont="Helvetica" --variable monofont="Menlo" --variable fontsize=11pt
ENGINE = --pdf-engine=pdflatex

all: $(INPUT)
	$(CMD) $(FLAGS) $(INPUT) $(ENGINE) --toc -o $(OUTPUT)
