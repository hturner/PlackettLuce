pladmm_mob_fit <- function (y, worth, x = NULL, start = NULL, weights = NULL,
                            offset = NULL, ..., estfun = FALSE, object = FALSE) {
    # do not use `x` as in partykit::mob as this assumes x covariates
    # are partitioned along with response and partitioning variables
    x <- !(is.null(x) || NCOL(x) == 0L)
    offset <- !is.null(offset)
    if (x || offset)
        warning("unused argument(s): ",
                paste(c("x"[x], "offset"[offset]), collapse = ","))
    dots <- list(...)
    if (!is.null(weights))
        warning("`weights` not yet implemented")

    orderings <- as.matrix(as.rankings(y))
    res <- pladmm_fit(orderings,
                      X = worth, start = start, ...)

    if (object){
        # return dummy PLADMM object so works with methods e.g. vcov, AIC
        res$x <- worth
        res$orderings <- orderings
        res$rank <- ncol(worth) - 1
        class(res) <- "PLADMM"
    }
    if (estfun) {
        if (!object){
            # add in extra required info as would be in PLADMM object
            res$x <- worth
            res$orderings <- orderings
        }
        percomp <- estfun.PLADMM(res)
        estfun <- rowsum(as.matrix(percomp), attr(y, "index"))
    } else estfun <- NULL
    list(coefficients = coef(res),
         objfun = -tail(res$loglik, 1),
         estfun = estfun,
         object = if (object) res else NULL)
}

# log-likelihood derivatives (score function) per ranking
#' @method estfun PLADMM
#' @importFrom matrixStats rowCumsums
#' @importFrom sandwich estfun
#' @export
estfun.PLADMM <- function(x, ...) {
    # get worth estimated from beta coefficients
    alpha <- x$tilde_pi
    # get design matrix (first derivatives wrt coef)
    X <- x$x
    # get reverse orderings
    n_item <- nrow(X)
    revorder <- x$orderings[, rev(seq_len(n_item))]
    # set item 0 to Inf so that returns NA when used as index
    revorder[revorder == 0] <- n_item + 1

    # get item worth for items in reverse order
    n_rankings <- nrow(revorder)
    A <- matrix(c(alpha, 0)[c(revorder)], nrow = n_rankings)
    # get derivative wrt to each estimable coef
    n_coef <- length(x$coefficients)
    res <- matrix(nrow = n_rankings, ncol = n_coef,
                  dimnames = list(NULL, colnames(X)))
    for (j in seq_len(n_coef)[-1]){ #intercept not estimable
        Xj <- matrix(c(X[,j], 0)[c(revorder)], nrow = n_rankings)
        ## part 1: derivative of first term
        p1 <- rowSums(Xj, na.rm = TRUE)
        ## part 2: derivative of second term
        p2 <- rowSums(rowCumsums(Xj*A)/rowCumsums(A), na.rm = TRUE)
        res[,j] <- p1 - p2
    }
    res[,-1]
}

