export ROOT=$(dir $(abspath $(CURDIR)))

main:
	Rscript analysis/main.r && echo '[OK]'

download-main:
	python3 admin/download.py main && echo '[OK]'

download-api:
	python3 admin/download.py api && echo '[OK]'

valex:
	python3 admin/valex.py && echo '[OK]'
