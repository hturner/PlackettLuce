# render vignette - result is in inst/doc
rmarkdown::render("vignettes/Overview.Rmd")

rmarkdown::render("vignettes/Overview.Rmd", "BiocStyle::pdf_document")

library(devtools)
library(pkgdown)
source_gist("https://gist.github.com/hturner/3152081e223ade0bb212bcef19f183bf",
            filename = "build_rmarkdown_format.R")

# either a) rebuild whole pkgdown site including help files
build_site()

# or b) just rebuild part
build_articles()

build_home()

build_reference_index()
