full:
	make tex ; make bib ; make tex ; make tex

tex:
	pdflatex main

bib:
	for aux in *.aux; do bibtex $$aux; done

cover:
	cd cl && pdflatex cover

words:
	cd wc && python3 wc.py

submit:
	cd sub && ./submit.sh
