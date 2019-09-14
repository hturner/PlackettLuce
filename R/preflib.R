#' Read Preflib Election Data Files
#'
#' Read orderings from `.soc`, `.soi`, `.toc` or `.toi` file types storing
#' election data as defined by
#' \href{http://www.preflib.org/}{\{PrefLib\}: A Library for Preferences}.
#'
#' The file types supported are
#' \describe{
#' \item{.soc}{Strict Orders - Complete List}
#' \item{.soi}{Strict Orders - Incomplete List}
#' \item{.toc}{Orders with Ties - Complete List}
#' \item{.toi}{Orders with Ties - Incomplete List}
#' }
#' Note that the file types do not distinguish between types of incomplete
#' orderings, i.e. whether they are a complete ranking of a subset of items
#' (as supported by [PlackettLuce()]) or top-\eqn{n} rankings of \eqn{n} items
#' from the full set of items (not currently supported by [PlackettLuce()]).
#'
#' The numerically coded orderings and their frequencies are read into a
#' data frame, storing the item names as an attribute. The
#' `as.aggregated_rankings` method converts these to an
#' [`"aggregated_rankings"`][aggregate.rankings] object with the items labelled
#' by the item names.
#'
#' A Preflib file may be corrupt, in the sense that the ordered items do not
#' match the named items. In this case, the file can be read is as a data
#' frame (with a warning) using the corresponding `read.*` function, but
#' `as.aggregated_rankings` will throw an error.
#' @return A data frame of class `"preflib"` with first column \code{Freq},
#' giving the frequency of the ranking in that row, and remaining columns
#' \code{Rank 1}, \ldots, \code{Rank p} giving the items ranked from first to
#' last place in that ranking. Ties are represented by vector elements in list
#' columns. The data frame has an attribute \code{"items"} giving the labels
#' corresponding to each item number.
#'
#' @param file An election data file, conventionally with extension `.soc`,
#' `.soi`, `.toc` or `.toi` according to data type.
#' @param x An object of class `"preflib"`.
#' @param ... Additional arguments passed to [as.rankings()]: `freq`,
#' `input` or `items` will be ignored with a warning as they are set
#' automatically.
#' @note The Netflix and cities datasets used in the examples are from
#' Caragiannis et al (2017) and Bennet and Lanning (2007) respectively. These
#' data sets require a citation for re-use.
#' @references
#' Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
#' \emph{Proceedings of Third International Conference on Algorithmic Decision
#' Theory (ADT 2013)}. Lecture Notes in Artificial Intelligence, Springer.
#'
#' Caragiannis, I., Chatzigeorgiou, X, Krimpas, G. A., and Voudouris, A. A.
#' (2017) Optimizing positional scoring rules for rank aggregation.
#' In \emph{Proceedings of the 31st AAAI Conference on Artificial Intelligence}.
#'
#' Bennett, J. and Lanning, S. (2007) The Netflix Prize.
#' \emph{Proceedings of The KDD Cup and Workshops}.
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

read.items <- function(file){ # read one line to find number of items
    if (!file.exists(file)) stop ("file does not exist")
    p <- as.integer(read.csv(file, nrows = 1L, header = FALSE))
    # get items
    items <- read.csv(file, skip = 1L, nrows = p, header = FALSE,
                      stringsAsFactors = FALSE, strip.white = TRUE)[,2L]
    names(items) <- seq_len(p)
    items
}

#' @importFrom utils count.fields
read.strict <- function(file, incomplete = FALSE){
    items <- read.items(file)
    # count maximum number of ranks
    if (incomplete){
        r <- max(count.fields(file, sep = ",")) - 1L
    } else r <- length(items)
    # read frequencies and ordered items
    nm <- c("Freq", paste("Rank", seq_len(r)))
    obs <- read.csv(file, skip = length(items) + 2L, header = FALSE,
                    col.names = nm, check.names = FALSE)
    preflib(obs, items)
}

#' @importFrom utils count.fields
read.ties <- function(file, incomplete = FALSE){
    items <- read.items(file)
    skip <- length(items) + 2L
    input <- chartr("{}", "''", readLines(file))
    # count maximum number of ranks (not needed for complete rankings)
    if (incomplete){
        r <- max(count.fields(textConnection(input), quote = "'",
                              sep = ",", skip = skip)) - 1L
    } else r <- length(items)
    # read counts and ordered items
    nm <- c("Freq", paste("Rank", seq_len(r)))
    obs <- read.csv(text = input, skip = skip, header = FALSE, quote = "'",
                    col.names = nm, check.names = FALSE,
                    na.strings = "", stringsAsFactors = FALSE)
    n <- nrow(obs)
    obs <- as.data.frame(lapply(obs, function(x) {
        if (is.character(x)) {
            x <- strsplit(x, ",")
            array(lapply(x, as.numeric), n)
        } else x }), check.names = FALSE)
    preflib(obs, items)
}

preflib <- function(obs, items){
    obs_items <- sort(unique(unlist(obs[, -1])))
    unnamed <- setdiff(as.character(obs_items), names(items))
    n <- length(unnamed)
    if (n) {
        warning("Corrupt file. Items with no name:\n",
                paste(unnamed[seq(min(n, 10L))], ", ..."[n > 10L],
                      collapse = ", "))
    }
    structure(obs, items = items, class = c("preflib", class(obs)))
}

#' @rdname preflib
#' @export
read.soc <- function(file){
    read.strict(file, incomplete = FALSE)
}

#' @rdname preflib
#' @export
read.soi <- function(file){
    # unused ranks will be NA
    read.strict(file, incomplete = TRUE)
}

#' @rdname preflib
#' @export
read.toc <- function(file){
    read.ties(file, incomplete = FALSE)
}

#' @rdname preflib
#' @export
read.toi <- function(file){
    # unused ranks will be NA
    read.ties(file, incomplete = TRUE)
}

#' @rdname preflib
#' @method as.aggregated_rankings preflib
#' @export
as.aggregated_rankings.preflib <- function(x, ...){
    nc <- ncol(x)
    if (identical(colnames(x), c("Freq", paste("Rank", seq(nc - 1))))){
        dots <- match.call(as.aggregated_rankings.preflib,
                           expand.dots = FALSE)[["..."]]
        ignore <- names(dots) %in% c("freq", "input", "items")
        if (any(ignore))
            warning("`freq`, `input` and `items` are set automatically for ",
                    "items of class \"preflib\"")
        dots <- dots[setdiff(names(dots), c("freq", "input", "items"))]
        do.call(as.rankings.matrix,
                c(list(as.matrix(x[, -1]), freq = x[, 1],
                       input = "orderings", items = attr(x, "items")), dots))
    } else stop("`x` is not a valid \"preflib\" object")
}
