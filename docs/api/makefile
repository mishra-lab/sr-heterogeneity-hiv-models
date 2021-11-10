full:
	pdflatex main ; for aux in *.aux; do bibtex $$aux; done ; pdflatex main

simple:
	pdflatex main

cover:
	cd cl && pdflatex cover

words:
	cd wc && python3 wc.py

submit:
	cd sub && ./submit.sh
