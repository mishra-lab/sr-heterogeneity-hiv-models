export ROOT=$(dir $(abspath $(CURDIR)))

main:
	Rscript analysis/main.r && echo '[OK]'

download:
	python3 admin/download.py && echo '[OK]'

valex:
	python3 admin/valex.py && echo '[OK]'
