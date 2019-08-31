#' Complete Orderings with the Missing Redundant Rank
#'
#' Given orderings with one rank missing, complete the ordering by assigning
#' the remaining item(s) to the final rank.
#'
#' @param orderings A data frame of orderings with one rank missing.
#' @param items A vector of item names.
#' @return A vector of the missing items, which will be a list if there are
#' any ties.
#'
#' @examples
#' # Orderings of 3 items, when only the best and worst are recorded
#' orderings <- data.frame(best = c("A", "B", "A"),
#'                         worst = c("C", "C", NA))
#' orderings$middle <- complete(orderings, items = c("A", "B", "C"))
#' @export
complete <- function(orderings, items){
    res <- vector(mode = "list", length = nrow(orderings))
    for (i in seq_along(res)){
        res[[i]] <- setdiff(items, unlist(orderings[i,]))
        if (!length(res[[i]])) res[[i]] <- NA
    }
    if (all(lengths(res) == 1)) {
        unlist(res)
    } else res
}
