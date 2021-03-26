full:
	pdflatex main ; for aux in *.aux; do bibtex $$aux; done ; pdflatex main

tex:
	pdflatex main

docx:
	cd .docx && python3 x.py && pandoc -s main.md -o main.docx

words:
	cd wc && python3 wc.py
