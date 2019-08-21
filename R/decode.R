#' Decode orderings
#'
#' @param orderings A data frame of coded orderings or an object that can be
#' coerced to such a data frame.
#' @param items A data frame of the items in each ranking, or a vector of
#' common items. The elements names are used as the key to the code if the
#' coded values in `orderings` are character, otherwise the indices of
#' the elements are used.
#' @return A data frame with the coded values replaced by the item labels.#'
#' @examples
#' # orderings of up to 3 items coded a A, B, C
#' orderings <- data.frame(Rank1 = c("A", "B"),
#'                         Rank2 = c("C", "A"),
#'                         Rank3 = c("B", NA),
#'                         stringsAsFactors = FALSE)
#' items <- data.frame(A = c("banana", "apple"),
#'                     B = c("orange", "pear"),
#'                     C = c("apple", NA),
#'                     stringsAsFactors = FALSE)
#' decode(orderings, items)
#'
#' # orderings with ties of up to 3 items, coded 1:3
#' orderings <- data.frame(Rank1 = c(1, 3),
#'                         Rank2 = I(list(c(2, 3), 2)),
#'                         Rank3 = c(NA, 1),
#'                         stringsAsFactors = FALSE)
#' items <- data.frame(A = c("banana", "apple"),
#'                     B = c("orange", "pear"),
#'                     C = c("apple", "orange"),
#'                     stringsAsFactors = FALSE)
#' decode(orderings, items)
#'
#' # same items in each comparison
#' items <- c(A = "banana", B = "orange", C = "pear")
#' decode(orderings, items)
#' @export
decode <- function(orderings,
                   items){
    orderings <- as.data.frame(orderings, stringsAsFactors = FALSE)
    items <- as.data.frame(items, stringsAsFactors = FALSE)
    nr <- nrow(orderings)
    nc <- ncol(orderings)
    seqr <- seq_len(nr)
    if (!is.null(dim(items)) && nrow(items) != nr)
        stop("items must be provided for every ordering, ",
             "nrow(items) should be: ", nr)
    if (mode(orderings[[1]]) == "character") {
        code <- names(items)
    } else {
        if (is.null(dim(items))) {
            code <- seq_along(items)
        } else code <- seq_len(ncol(items))
    }
    # case 1 no list columns (ties)
    for (k in seq_len(nc)){
        if (is.list(orderings[[k]])) {
            nk <- lengths(orderings[[k]])
            i <- rep(seqr, nk)
            res <- split(decode_vector(unlist(orderings[[k]]), items, code, i),
                         i)
        } else {
            res <- decode_vector(orderings[[k]], items, code, seqr)
        }
        if (any(is.na(res) != is.na(orderings[[k]])))
            stop("Rank ", k, " item(s) missing for orderings: ",
                 paste(which(!is.na(orderings[[k]]) & is.na(res)),
                       collapse = ", "))
        orderings[[k]] <- res
    }
    orderings
}

decode_vector <- function(x, items, code, i){
    j <- match(x, code, nomatch = 0L)
    if (is.data.frame(items)){
        x[j != 0] <- items[cbind(i, j)]
    } else x[j != 0] <- items[j]
    x
}


#' Complete orderings with redundant rank
#'
#' Given orderings with one rank missing, complete the ordering by assigning
#' the remaining item(s) to the final rank.
#'
#' @param orderings A data frame of orderings with one rank missing.
#' @param items A vector of item names.
#' @return A data frame wit
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
