# render vignette - result is in vignettes_tmp_output
rmarkdown::render("vignettes/Overview.Rmd", output_dir = "vignettes_tmp_output",
                  clean = FALSE)

rmarkdown::render("vignettes/Overview.Rmd", "bookdown::html_document2",
                  output_dir = "vignettes_tmp_output",
                  clean = FALSE)

## ! LaTeX Error: Environment cslreferences undefined.
rmarkdown::render("vignettes/Overview.Rmd", "BiocStyle::pdf_document",
                  output_dir = "vignettes_tmp_output",
                  clean = FALSE)

library(devtools)
library(pkgdown)

# either a) rebuild whole pkgdown site including help files
build_site(lazy = FALSE)
file.remove("vignettes/Overview.knit.md")
file.remove("vignettes/Overview.utf8.md")

# or b) just rebuild part (may need to do to still get tab refs)
build_articles(lazy = FALSE) #vignette

build_home()

build_news()

build_reference_index()

build_reference() #Rd files
