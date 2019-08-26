#' Read Preflib Election Data Files
#'
#' Read orderings from `.soc`, `.soi`, `.toc` or `.toi` file types storing
#' election data as defined by
#' \href{http://www.preflib.org}{\{PrefLib\}: A Library for Preferences}.
#'
#' @param file An election data file, conventionally with extension `.soc`,
#' `.soi`, `.toc` or `.toi` according to data type.
#'
#' The file types supported are
#' \describe{
#' \item{.soc}{Strict Orders - Complete List}
#' \item{.soi}{Strict Orders - Incomplete List}
#' \item{.toc}{Orders with Ties - Complete List}
#' \item{.toi}{Orders with Ties - Incomplete List}
#' }
#' Note that the file types do not distinguish between types of incomplete
#' orderings, i.e. whether they are a complete ranking of a subset of item
#' (as supported by [PlackettLuce()]) or top-$n$ rankings of $n$ items from
#' the full set of items (not currently supported by [PlackettLuce()]).
#'
#' The numerically coded orderings and their frequencies are read into a
#' data frame, storing the item name as an attribute. The
#' `as.rankings` method converts these to an `"aggregated_rankings"` object
#' with the items labelled by the item names.
#' @return A data frame of class `"preflib"` with first column \code{Freq},
#' giving the frequency of the ranking in that row, and remaining columns
#' \code{Rank 1} \ldots \code{Rank p} giving the items ranked from first to
#' last place in that ranking. Ties are represented by vector elements in list
#' columns. The data frame has an attribute \code{"items"} giving the labels
#' corresponding to each item number.
#' @references
#' Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
#' \emph{Proceedings of Third International Conference on Algorithmic Decision
#' Theory (ADT 2013)}. Lecture Notes in Artificial Intelligence, Springer.
#'
#' @examples
#'
#' # can take a little while depending on speed of internet connection
#'
#' \dontrun{
#' # url for preflib data in the "Election Data" category
#' preflib <- "http://www.preflib.org/data/election/"
#'
#' # strict complete orderings of four films on Netflix
#' netflix <- read.soc(file.path(preflib, "netflix/ED-00004-00000101.soc"))
#' head(netflix)
#' attr(netflix, "items")
#'
#' head(as.rankings(netflix))
#'
#' # strict incomplete orderings of 6 random cities from 36 in total
#' cities <- read.soi(file.path(preflib, "cities/ED-00034-00000001.soi"))
#'
#' # strict incomplete orderings of drivers in the 1961 F1 races
#' # 8 races with 17 to 34 drivers in each
#' f1 <- read.soi(file.path(preflib, "f1/ED-00010-00000001.soi"))
#'
#' # complete orderings with ties of 30 skaters
#' skaters <- read.toc(file.path(preflib, "skate/ED-00006-00000001.toc"))
#'
#' # incomplete orderings with ties of 10 sushi items from 100 total
#' # orderings were derived from numeric ratings
#' sushi <- read.toi(file.path(preflib, "sushi/ED-00014-00000003.toi"))
#' }
#' @importFrom utils read.csv
#' @name preflib
NULL

#' @rdname preflib
#' @export
read.soc <- function(file){
    # read one line to find number of items
    p <- as.integer(read.csv(file, nrows = 1L, header = FALSE))
    # get items
    items <- read.csv(file, skip = 1L, nrows = p, header = FALSE,
                      stringsAsFactors = FALSE, strip.white = TRUE)[,2L]
    names(items) <- seq_len(p)
    # read counts and ordered items
    obs <- read.csv(file, skip = p + 2L, header = FALSE,
                    check.names = FALSE)
    colnames(obs) <- c("Freq", paste("Rank", seq_len(ncol(obs) - 1)))
    structure(obs, items = items, class = c("preflib", class(obs)))
}

#' @rdname preflib
#' @export
read.soi <- function(file){
    # keep unused ranks as NA
    read.soc(file)
}

#' @rdname preflib
#' @export
read.toc <- function(file){
    # read one line to find number of items
    p <- as.integer(read.csv(file, nrows = 1L, header = FALSE))
    # get items
    items <- read.csv(file, skip = 1L, nrows = p, header = FALSE,
                      stringsAsFactors = FALSE, strip.white = TRUE)[,2L]
    names(items) <- seq_len(p)
    # read counts and ordered items
    obs <- read.csv(text = chartr("{}", "''", readLines(file)),
                    skip = p + 2L, header = FALSE, quote = "'",
                    check.names = FALSE, stringsAsFactors = FALSE)
    colnames(obs) <- c("Freq", paste("Rank", seq_len(ncol(obs) - 1)))
    obs <- as.data.frame(sapply(obs, function(x) {
        x <- strsplit(as.character(x), ",")
        sapply(x, as.numeric)}))
    structure(obs, items = items, class = c("preflib", class(obs)))
}

#' @rdname preflib
#' @export
read.toi <- function(file){
    read.toc(file)
}

#' @rdname preflib
#' @export
as.rankings.preflib <- function(x, verbose = TRUE, ...){
    nc <- ncol(x)
    if (identical(colnames(x), c("Freq", paste("Rank", seq(nc - 1))))){
        dots <- match.call(as.rankings.preflib, expand.dots = FALSE)[["..."]]
        ignore <- names(dots) %in% c("freq", "input", "items")
        if (any(ignore))
            warning("`freq`, `input` and `items` are set automatically for ",
                    "items of class \"preflib\"")
        dots <- dots[setdiff(names(dots), c("freq", "input", "items"))]
        do.call(as.rankings.matrix,
                c(list(as.matrix(x[, -1]), freq = x[, 1],
                       input = "orderings", items = attr(x, "items"),
                       verbose = verbose), dots))
    } else {
        as.rankings.matrix(as.matrix(x), verbose = verbose, ...)
    }
}
