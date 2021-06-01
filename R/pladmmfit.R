# log-likelihood derivatives (score function) per ranking
#' @method estfun PLADMM
#' @importFrom matrixStats rowCumsums
#' @importFrom sandwich estfun
#' @export
estfun.PLADMM <- function(x, ref = 1L, ...) {
    # get worth estimated from beta coefficients
    alpha <- x$tilde_pi
    # get design matrix (first derivatives wrt coef)
    X <- x$x
    # get reverse orderings
    n_item <- nrow(X)
    revorder <- x$orderings[, rev(seq_len(n_item))]
    # set item 0 to Inf so that returns NA when used as index
    revorder[revorder == 0] <- Inf

    # get item worth for items in reverse order
    n_rankings <- nrow(revorder)
    A <- matrix(alpha[c(revorder)], nrow = n_rankings)
    # get derivative wrt to each estimable coef
    n_coef <- length(x$coefficients)
    res <- matrix(nrow = n_rankings, ncol = n_coef,
                  dimnames = list(NULL, colnames(X)))
    for (j in seq_len(n_coef)[-1]){ #intercept not estimable
        Xj <- matrix(c(X[,j], 0)[c(revorder)], nr = n_rankings)
        ## part 1: derivative of first term
        p1 <- rowSums(Xj, na.rm = TRUE)
        ## part 2: derivative of second term
        p2 <- rowSums(rowCumsums(Xj*A)/rowCumsums(A), na.rm = TRUE)
        res[,j] <- p1 - p2
    }
    res[,-1]
}

