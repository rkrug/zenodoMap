.PHONY: deploy test check pkgdown render-readme release

test:
	R -q -e "devtools::test()"

check:
	R -q -e "devtools::check()"

pkgdown:
	R -q -e "pkgdown::build_site()"

render-readme:
	R -q -e "if (file.exists('README.qmd')) quarto::quarto_render('README.qmd', output_file='README.md')"

deploy:
	R -q -e "rsconnect::deployApp(appDir='.', appFiles=c('app.R','R'))"

release: test deploy
