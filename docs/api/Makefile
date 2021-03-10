full:
	pdflatex main && bibtex main && pdflatex main

tex:
	pdflatex main

docx:
	cd .docx && python3 x.py && pandoc -s main.md -o main.docx