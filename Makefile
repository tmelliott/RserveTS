default: README.md

rfiles: $(wildcard R/*.R)

document:
	Rscript -e "devtools::document()"

install: document
	R CMD INSTALL .

README.md: README.Rmd install
	Rscript -e "rmarkdown::render('README.Rmd')"
	@rm README.html

test:
	Rscript -e "devtools::test()"
