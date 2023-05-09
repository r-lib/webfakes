
all: README.md inst/examples/httpbin/assets/httpbin.html

README.md: README.Rmd
	Rscript -e "library(knitr); knit('$<', output = '$@', quiet = TRUE)"

# sudo npm install -g @redocly/cli
inst/examples/httpbin/assets/httpbin.html: inst/examples/httpbin/openapi.yaml
	redocly build-docs $< -o $@
