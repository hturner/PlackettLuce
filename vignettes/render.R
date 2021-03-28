# render vignette - result is in vignettes_tmp_output
rmd <- "vignettes/PLADMM.Rmd"
rmarkdown::render(rmd, output_dir = "vignettes_tmp_output",
                  clean = FALSE)

rmarkdown::render(rmd, "bookdown::html_document2",
                  output_dir = "vignettes_tmp_output",
                  clean = FALSE)

## ! LaTeX Error: Environment cslreferences undefined.
rmarkdown::render(rmd, "BiocStyle::pdf_document",
                  output_dir = "vignettes_tmp_output",
                  clean = FALSE)

library(devtools)
library(pkgdown)

# either a) rebuild whole pkgdown site including help files
build_site(lazy = FALSE)

# or b) just rebuild part (may need to do to still get tab refs)
build_articles(lazy = FALSE) #vignette

build_home()

build_news()

build_reference_index()

build_reference() #Rd files
