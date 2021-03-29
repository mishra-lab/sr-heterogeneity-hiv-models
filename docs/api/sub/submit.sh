from=../main.pdf
submit=./aids/1
pdftk $from cat  1     output $submit/title.pdf
pdftk $from cat  2     output $submit/abstract.pdf
pdftk $from cat  3-24  output $submit/body.pdf
pdftk $from cat 25-end output $submit/appendix.pdf
cp cl/cover.pdf $submit/cover.pdf
