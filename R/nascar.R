#' Results from 2002 NASCAR Season
#'
#' This is an example dataset from \cite{Hunter 2004} recording the results of
#' 36 car races in the 2002 NASCAR season in the United States. Each record is
#' an ordering of the drivers according to their finishing position.
#'
#' @format A matrix with 36 rows corresponding to the races and 43 columns
#' corresponding to the positions. The columns contain the ID for the driver
#' that came first to last place respectively. The \code{"drivers"}
#' attribute contains the names of the 87 drivers.
#' @references
#' Hunter, D. R. (2004) MM algorithms for generalized Bradley-Terry models.
#' \emph{The Annals of Statistics}, \bold{32(1)}, 384--406.
#' @examples
#'
#' # convert orderings to rankings
#' nascar[1:2, ]
#' R <- as.rankings(nascar, input = "orderings",
#'                  items = attr(nascar, "drivers"))
#' R[1:2, 1:4, as.rankings = FALSE]
#' format(R[1:2], width = 60)
#'
#' # fit model as in Hunter 2004, excluding drivers that only lose
#' keep <- seq_len(83)
#' R2 <- R[, keep]
#' mod <- PlackettLuce(R2, npseudo = 0)
#'
#' # show coefficients as in Table 2 of Hunter 2004
#' avRank <- apply(R, 2, function(x) mean(x[x > 0]))
#' coefs <- round(coef(mod)[order(avRank[keep])], 2)
#' head(coefs, 3)
#' tail(coefs, 3)
"nascar"
