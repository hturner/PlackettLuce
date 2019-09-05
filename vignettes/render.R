# render vignette - result is in vignettes_tmp_output
rmarkdown::render("vignettes/Overview.Rmd", output_dir = "vignettes_tmp_output",
                  clean = FALSE)

rmarkdown::render("vignettes/Overview.Rmd", "BiocStyle::pdf_document",
                  output_dir = "vignettes_tmp_output")

library(devtools)
library(pkgdown)
# no longer works, just hack current pkgdown function
#source_gist("https://gist.github.com/hturner/3152081e223ade0bb212bcef19f183bf",
#            filename = "build_rmarkdown_format.R")

assignInNamespace(
    "build_rmarkdown_format",
    ns = "pkgdown",
    value = function(pkg = ".",
                     name,
                     depth = 1L,
                     data = list(),
                     toc = TRUE) {
        template <- pkgdown:::rmarkdown_template(pkg, name, depth = depth,
                                       data = data)
        out <- bookdown::html_document2(toc = toc, toc_depth = 2,
                                        self_contained = FALSE, theme = NULL,
                                        template = template$path)
        out$knitr$opts_chunk <- pkgdown:::fig_opts_chunk(pkg$figures, out$knitr$opts_chunk)
        attr(out, "__cleanup") <- template$cleanup
        out
    }
)

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
