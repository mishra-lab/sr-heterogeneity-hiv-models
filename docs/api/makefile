full:
	make clean ; make tex ; make bib ; make tex ; make tex ; make words

tex:
	pdflatex main

bib:
	for aux in *.aux; do bibtex $$aux; done

words:
	cd wc && python3 wc.py

clean:
	texclean .