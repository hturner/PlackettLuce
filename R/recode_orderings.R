#' Title
#'
#' @param orderings
#' @param items
#' @param code
#'
#' @return
#' @export
#'
#' @examples
#' orderings <- data.frame(Rank1 = c("A", "B"),
#'                         Rank2 = c("C", "A"),
#'                         Rank3 = c("B", NA),
#'                         stringsAsFactors = FALSE)
#' items <- data.frame(ItemA = c("banana", "apple"),
#'                     ItemB = c("orange", "pear"),
#'                     ItemC = c("apple", NA),
#'                     stringsAsFactors = FALSE)
#' recode_orderings(orderings, items)
#'
#' orderings <- data.frame(Rank1 = c("A", "C"),
#'                         Rank2 = I(list(c("B", "C"), "B")),
#'                         Rank3 = c(NA, "A"),
#'                         stringsAsFactors = FALSE)
#' items <- data.frame(ItemA = c("banana", "apple"),
#'                     ItemB = c("orange", "pear"),
#'                     ItemC = c("apple", "orange"),
#'                     stringsAsFactors = FALSE)
recode_orderings <- function(orderings,
                             items,
                             code = sort(unique(unlist(orderings)))){
    nr <- nrow(orderings)
    nc <- ncol(orderings)
    seqr <- seq_len(nr)
    if (is.data.frame(items) && nrow(items) != nr)
        stop("items must be provided for every ordering, ",
             "nrow(items) should be: ", nr)
    # case 1 no list columns (ties)
    for (k in seq_len(nc)){
        if (is.list(orderings[[k]])) {
            nk <- lengths(orderings[[k]])
            i <- rep(seqr, nk)
            res <- split(recode_vector(unlist(orderings[[k]]), items, code, i),
                         i)
        } else {
            res <- recode_vector(orderings[[k]], items, code, seqr)
        }
        if (any(is.na(res) != is.na(orderings[[k]])))
            stop("Rank ", k, " item(s) missing for orderings: ",
                 paste(which(!is.na(orderings[[k]]) & is.na(res)),
                       collapse = ", "))
        orderings[[k]] <- res
    }
    orderings
}

recode_vector <- function(x, items, code, i){
    j <- match(x, code, nomatch = 0L)
    if (is.data.frame(items)){
        x[j != 0] <- items[cbind(i, j)]
    } else x[j != 0] <- items[j]
    x
}


#' Title
#'
#' @param orderings
#' @param code
#'
#' @return
#' @export
#'
#' @examples
complete_orderings <- function(orderings, code){
    res <- vector(mode = "list", length = nrow(orderings))
    for (i in seq_along(res)){
        res[[i]] <- setdiff(code, unlist(orderings[i,]))
        if (!length(res[[i]])) res[[i]] <- NA
    }
    if (all(lengths(res) == 1)) {
        unlist(res)
    } else res
}
