# render vignette - result is in inst/doc
library(devtools)
build_vignettes()

# create version for pkgdown site (not needed )
library(pkgdown)

# either a) rebuild whole pkgdown site including help files
build_site()

# or b) just rebuild vignette
build_articles() # or build_site()

# fix up result
source_gist("https://gist.github.com/hturner/5202ee24e7f2db02ed9ab4b2e1805dac",
            filename = "fix_biocstyle_articles.r")
fix_biocstyle_articles()

