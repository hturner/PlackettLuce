# render vignette - result is in inst/doc
rmarkdown::render("vignettes/Overview.Rmd", output_dir = "vignettes_tmp_output")

rmarkdown::render("vignettes/Overview.Rmd", "BiocStyle::pdf_document",
                  output_dir = "vignettes_tmp_output")

library(devtools)
library(pkgdown)
source_gist("https://gist.github.com/hturner/3152081e223ade0bb212bcef19f183bf",
            filename = "build_rmarkdown_format.R")

# either a) rebuild whole pkgdown site including help files
build_site()
file.remove("vignettes/Overview.knit.md")
file.remove("vignettes/Overview.utf8.md")

# or b) just rebuild part
build_articles() #vignette

build_home()

build_reference_index()

build_reference()
