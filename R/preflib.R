#' Read Preflib Election Data Files
#'
#' Read orderings from `.soc`, `.soi`, `.toc` or `.toi` file types storing
#' election data as defined by
#' \href{https://www.preflib.org/}{\{PrefLib\}: A Library for Preferences}.
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
#' match the named items. In this case, the file can be read in as a data
#' frame (with a warning) using the corresponding `read.*` function, but
#' `as.aggregated_rankings` will throw an error.
#' @return A data frame of class `"preflib"` with first column \code{Freq},
#' giving the frequency of the ranking in that row, and remaining columns
#' \code{Rank 1}, \ldots, \code{Rank r} giving the items ranked from first to
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
#' Bennet and Lanning (2007) and Caragiannis et al (2017) respectively. These
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
#' # strict complete orderings of four films on Netflix
#' netflix <- read.soc(system.file("extdata", "netflix.soc",
#'                                 package = "PlackettLuce"))
#' head(netflix)
#' attr(netflix, "items")
#'
#' head(as.aggregated_rankings(netflix))
#'
#' # strict incomplete orderings of 6 random cities from 36 in total
#' cities <- read.soi(system.file("extdata", "cities.soi",
#'                                 package = "PlackettLuce"))
#'
#' # complete orderings with ties of 30 skaters
#' skaters <- read.toc(system.file("extdata", "skaters.toc",
#'                                 package = "PlackettLuce"))
#'
#' # incomplete orderings with ties: most important qualities for success
#' # from 20 in total
#' qualities <- read.toi(system.file("extdata", "education_qualities.toi",
#'                       package = "PlackettLuce"))
#'
#' # alternatively read from a url
#' # - can take a little while depending on speed of internet connection
#'
#' \dontrun{
#' # incomplete orderings with ties: most important qualities for success
#' # from 20 in total
#' preflib <- "https://www.preflib.org/static/data/"
#' qualities2 <- read.toi(file.path(preflib, "education/00032-00000007.toi"))
#' all.equal(qualities, qualities2)
#' }
#' @importFrom utils read.csv
#' @name preflib
NULL

read_text <- function(file){
    if (!nchar(file))
        stop("`file` is an empty string: Please supply a valid file path")
    test  <- tryCatch(file(file, "rt"), silent = TRUE,
                     warning = function(w) w, error = function(e) e)
    if (!inherits(test, "connection")){
        stop(test$message, call. = FALSE)
    } else close(test)
    readLines(file, encoding = "UTF-8")
}

get_items <- function(txt){
    id <- grep("^# ALTERNATIVE NAME", txt)
    nm <- sub("^# ALTERNATIVE NAME [0-9]+: ", "", txt[id])
    n <- as.numeric(sub("^# ALTERNATIVE NAME ([0-9]+): .*", "\\1", txt[id]))
    items <- character(max(n))
    items[n] <- nm
    names(items) <- seq_along(items)
    items
}

read_preflib <- function(file, ties = FALSE){
    # read all lines as text
    txt <- read_text(file)
    # get item names from metadata
    meta <- grep("^#", txt)
    items <- get_items(txt[meta])
    # read counts and ordered items (count separated by ":", any ties in {})
    txt <- chartr(":{}", ",''", txt[-meta])
    txtConn <- textConnection(txt)
    obs <- read.csv(txtConn, header = FALSE, quote = "'", check.names = FALSE,
                    na.strings = "", stringsAsFactors = FALSE)
    close(txtConn)
    colnames(obs) <- c("Freq", paste("Rank", seq_len(ncol(obs) - 1)))
    # handle any ties (don't use array list as dim attribute kept on MacOS)
    if (ties){
        rank_class <- vapply(obs, is.character, logical(1))
        for (i in which(rank_class)){
            obs[[i]] <- lapply(strsplit(obs[[i]], ","), as.numeric)
        }
    }
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
    read_preflib(file)
}

#' @rdname preflib
#' @export
read.soi <- function(file){
    # unused ranks will be NA
    read_preflib(file)
}

#' @rdname preflib
#' @export
read.toc <- function(file){
    read_preflib(file, ties = TRUE)
}

#' @rdname preflib
#' @export
read.toi <- function(file){
    # unused ranks will be NA
    read_preflib(file, ties = TRUE)
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

#' @method as.rankings preflib
#' @export
as.rankings.preflib <- function(x, ...){
    stop("\"preflib\" objects hold aggregated rankings: ",
         "use as.aggregated_rankings() instead.")
}
