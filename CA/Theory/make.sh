pandoc -N --variable "geometry=margin=1.1in" --variable mainfont="Palatino" --variable sansfont="Helvetica" --variable monofont="Menlo" --variable fontsize=11pt Computer_Architecture_Notes.md --pdf-engine=pdflatex --toc -o Computer_Architecture_Notes.pdf

evince Computer_Architecture_Notes.pdf &
