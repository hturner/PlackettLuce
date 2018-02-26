#' Read Strict Orders - Complete List Data from File
#'
#' Read data from a "Strict Orders - Complete List" file, as provided by
#' \href{http://www.preflib.org}{\{PrefLib\}: A Library for Preferences}.
#'
#' @param file a path or url for the Strict Orders - Complete List file,
#' conventionally with a \code{.soc} extension.
#'
#' @return a data frame with first column \code{n}, giving the frequency of
#' the ranking in that row, and remaining columns \code{Rank 1} \ldots
#' \code{Rank p} giving the items ranked from first to last place in that
#' ranking. The data frame has an attribute \code{"item"} giving the labels
#' corresponding to each item number.
#' @references
#' Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
#' \emph{Proceedings of Third International Conference on Algorithmic Decision
#' Theory (ADT 2013)}. Lecture Notes in Artificial Intelligence, Springer.
#'
#' @examples
#'
#' # can take a little while depending on speed of internet connection
#' \dontrun{
#' # url for preflib data in the "Election Data" category
#' preflib <- "http://www.preflib.org/data/election/"
#'
#' # strict orderings of four films on Netflix
#' netflix <- read.soc(file.path(preflib, "netflix/ED-00004-00000101.soc"))
#' head(netflix)
#' attr(netflix, "item")
#' }
#'
#' @importFrom utils read.csv
#' @export
read.soc <- function(file){
    # read one line to find number of items
    p <- as.integer(read.csv(file, nrows = 1, header = FALSE))
    # get items
    item <- read.csv(file, skip = 1, nrows = p, header = FALSE,
                     stringsAsFactors = FALSE, strip.white = TRUE)[,2]
    names(item) <- seq_len(p)
    # read counts and ordered items
    structure(read.csv(file, col.names = c("n", paste("Rank", seq_len(p))),
                       skip = p + 2, header = FALSE,
                       check.names = FALSE),
              item = item)
}

read.soi <- function(file){
    # read one line to find number of items
    p <- as.integer(read.csv(file, nrows = 1, header = FALSE))
    # get items
    item <- read.csv(file, skip = 1, nrows = p, header = FALSE,
                     stringsAsFactors = FALSE, strip.white = TRUE)[,2]
    names(item) <- seq_len(p)
    # read counts and ordered items
    structure(read.csv(file, col.names = c("n", paste("Rank", seq_len(p))),
                       skip = p + 2, header = FALSE,
                       check.names = FALSE),
              item = item)
}
