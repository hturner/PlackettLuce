#' Preferred Bean Varieties in Nicaragua
#'
#' This is a subset of data from trials of bean varieties in Nicaragua over five
#' growing seasons. Farmers were asked to try three varieties of bean from a
#' total of ten varieties and to rank them in order of preference. In addition,
#' for each variety the farmers were asked to compare each trial variety to the
#' local variety and state whether they considered it to be better or worse.
#'
#' There are three crop seasons in Central America:
#' \describe{
#'     \item{Primera}{May - August.}
#'     \item{Postrera}{September - October.}
#'     \item{Apante}{November - January.}
#' }
#' Beans can be planted near the beginning of each season, though are most
#' commonly planted in the Postrera or Apante seasons.
#'
#' @format A data frame with 842 records and 11 variables:
#' \describe{
#'     \item{\code{variety_a}}{The name of variety A in the comparison.}
#'     \item{\code{variety_b}}{The name of variety B in the comparison.}
#'     \item{\code{variety_c}}{The name of variety C in the comparison.}
#'     \item{\code{best}}{The variety the farmer ranked in first place ("A",
#'     "B" or "C").}
#'     \item{\code{worst}}{The variety the farmer ranked in last place ("A",
#'     "B" or "C").}
#'     \item{\code{var_a}}{How the farmer ranked variety A compared to the local
#'     variety ("Worse" or "Better").}
#'     \item{\code{var_b}}{How the farmer ranked variety B compared to the local
#'     variety ("Worse" or "Better").}
#'     \item{\code{var_c}}{How the farmer ranked variety C compared to the local
#'     variety ("Worse" or "Better").}
#'     \item{\code{season}}{A factor specifying the growing season ("Po - 15",
#'     "Ap - 15", "Pr - 16", "Po - 16", "Ap - 16".}
#'     \item{\code{year}}{The year of planting.}
#'     \item{\code{maxTN}}{The maximum temperature at night during the
#'     vegetative cycle (degrees Celsius).}
#' }
#' @source The data were provided by Bioversity International, a CGIAR research
#' centre \url{https://www.bioversityinternational.org}.
#' @examples
#'
#' # Consider the best and worst rankings. These give the variety the
#' # farmer thought was best or worst, coded as A, B or C for the
#' # first, second or third variety assigned to the farmer
#' # respectively.
#' data(beans)
#' head(beans[c("best", "worst")], 2)
#'
#' # Convert these to numeric values, allowing us to impute the
#' # middle-ranked variety (a strict ranking is assumed here, so the
#' # sum of each row should be 6)
#' beans <- within(beans, {
#'    best <- match(best, c("A", "B", "C"))
#'    worst <- match(worst, c("A", "B", "C"))
#'    middle <- 6 - best - worst
#' })
#' head(beans[c("best", "middle", "worst")], 2)
#'
#' # This gives an ordering of the three varieties the farmer was
#' # given. The names of these varieties are stored in separate
#' # columns
#' varieties <- as.matrix(beans[c("variety_a", "variety_b", "variety_c")])
#' head(varieties, 2)
#'
#' # So we can convert the variety IDs to the variety names
#' n <- nrow(beans)
#' beans <- within(beans, {
#'     best <- varieties[cbind(seq_len(n), best)]
#'     worst <- varieties[cbind(seq_len(n), worst)]
#'     middle <- varieties[cbind(seq_len(n), middle)]
#' })
#' head(beans[c("best", "middle", "worst")], 2)
#'
#' # Create a rankings object from the rankings of order three
#' ## each ranking is a sub-ranking of three varieties from the full set
#' lab <- c("Local", sort(unique(as.vector(varieties))))
#' R <- as.rankings(beans[c("best", "middle", "worst")],
#'                  input = "ordering", labels = lab)
#' head(R)
#'
#' # Convert worse/better columns to ordered pairs
#' head(beans[c("var_a", "var_b", "var_c")], 2)
#' paired <- list()
#' for (id in c("a", "b", "c")){
#'     ordering <- matrix("Local", nrow = n, ncol = 2)
#'     worse <- beans[[paste0("var_", id)]] == "Worse"
#'     ## put trial variety in column 1 when it is not worse than local
#'     ordering[!worse, 1] <- beans[[paste0("variety_", id)]][!worse]
#'     ## put trial variety in column 2 when it is worse than local
#'     ordering[worse, 2] <- beans[[paste0("variety_", id)]][worse]
#'     paired[[id]] <- ordering
#' }
#' head(paired[["a"]])
#'
#' # Convert orderings to sub-rankings of full set and combine all rankings
#' paired <- lapply(paired, as.rankings, input = "ordering", labels = lab)
#' R <- rbind(R, paired[["a"]], paired[["b"]], paired[["c"]])
#' head(R)
#' tail(R)
"beans"
