from=../main.pdf
submit=./tmp
# pdftk $from cat  2     output $submit/abstract.pdf # J/AIDS
# pdftk $from cat  1-24  output $submit/manuscript.pdf # JAIDS
# pdftk $from cat  1     output $submit/title.pdf # AIDS
# pdftk $from cat  3-24  output $submit/body.pdf # AIDS
# pdftk $from cat  1-24  output $submit/main.pdf # medRxiv
# pdftk $from cat 25-end output $submit/appendix.pdf # J/AIDS
cp $from $submit/manuscript.pdf # Epidemics
cp ../coi.txt $submit/coi.txt # Epidemics
cp cl/cover.pdf $submit/cover.pdf
