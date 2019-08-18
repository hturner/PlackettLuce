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
    p <- as.integer(read.csv(file, nrows = 1L, header = FALSE))
    # get items
    item <- read.csv(file, skip = 1L, nrows = p, header = FALSE,
                     stringsAsFactors = FALSE, strip.white = TRUE)[,2L]
    names(item) <- seq_len(p)
    # read counts and ordered items
    obs <- read.csv(file, skip = p + 2L, header = FALSE,
                    check.names = FALSE)
    colnames(obs) <- c("n", paste("Rank", seq_len(ncol(obs) - 1)))
    structure(obs, item = item, class = c("preflib", class(obs)))
}

#' Title
#'
#' @param file
#'
#' @return
#'
#' @examples
#' # url for preflib data in the "Election Data" category
#' preflib <- "http://www.preflib.org/data/election/"
#'
#' # strict orderings of 6 random cities from 36 in total
#' cities <- read.soi(file.path(preflib, "cities/ED-00034-00000001.soi"))
#'
#' # strict orderings of drivers in the 1961 F1 races
#' # 8 races with 17 to 34 drivers in each
#' f1 <- read.soi(file.path(preflib, "f1/ED-00010-00000001.soi"))
#' @export
read.soi <- function(file){
    # keep unused ranks as NA
    read.soc(file)
}


#' Title
#'
#' @param file
#'
#' @return
#'
#' @examples
#' @export
#' # url for preflib data in the "Election Data" category
#' preflib <- "http://www.preflib.org/data/election/"
#'
#' # orderings of 30 skaters, with ties
#' skaters <- read.toc(file.path(preflib, "skate/ED-00006-00000001.toc"))
read.toc <- function(file){
    # read one line to find number of items
    p <- as.integer(read.csv(file, nrows = 1L, header = FALSE))
    # get items
    item <- read.csv(file, skip = 1L, nrows = p, header = FALSE,
                     stringsAsFactors = FALSE, strip.white = TRUE)[,2L]
    names(item) <- seq_len(p)
    # read counts and ordered items
    obs <- read.csv(text = chartr("{}", "''", readLines(file)),
                     skip = p + 2L, header = FALSE, quote = "'",
                     check.names = FALSE, stringsAsFactors = FALSE)
    colnames(obs) <- c("n", paste("Rank", seq_len(ncol(obs) - 1)))
    obs <- as.data.frame(sapply(obs, function(x) {
        x <- strsplit(as.character(x), ",")
        sapply(x, as.numeric)}))
    structure(obs, item = item, class = c("preflib", class(obs)))
}

#' Title
#'
#' @param file
#'
#' @return
#'
#' @examples
#' @export
#' # url for preflib data in the "Election Data" category
#' preflib <- "http://www.preflib.org/data/election/"
#'
#' # orderings of 30 skaters, with ties
#' sushi <- read.toi(file.path(preflib, "sushi/ED-00014-00000003.toi"))
read.toi <- function(file){
    read.toc(file)
}
