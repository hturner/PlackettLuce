#' Decode Orderings using a Key to Item Names
#'
#' Decode orderings by replacing numeric or character coded values with item
#' names.
#'
#' @param orderings A data frame of coded orderings.
#' @param items A data frame of the items in each ranking, or a vector of
#' common items.
#' @param code (Optional) a vector giving the key to the code. If missing,
#' `names(items)` is used for a character code, while `seq(items)` is used
#' for a numeric code.
#' @return A data frame with the coded values replaced by the item names.
#' @examples
#' # orderings of up to 3 items coded as A, B, C
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
                   items,
                   code = NULL){
    if (!is.null(dim(items)))
        items <- as.data.frame(items, stringsAsFactors = FALSE)
    orderings <- as.data.frame(orderings, stringsAsFactors = FALSE)
    nr <- nrow(orderings)
    nc <- ncol(orderings)
    seqr <- seq_len(nr)
    if (!is.null(dim(items)) && nrow(items) != nr)
        stop("items must be provided for every ordering, ",
             "nrow(items) should be: ", nr)
    if (is.null(code)){
        if (mode(orderings[[1]]) == "character") {
            code <- names(items)
        } else code <-seq(items)
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
