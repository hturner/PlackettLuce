#' Wrapper around pladmm for Model-based Recursive Partitioning
#'
#' This is a wrapper around \code{pladmm} as required by
#' \code{\link[partykit]{mob}} for model-based recursive partitioning. It is
#' not intended for general use.
#'
#' @param y a \code{"\link{grouped_rankings}"} object giving the rankings to
#' model.
#' @param x model matrix specifying the dependence of item worth on covariates.
#' @inheritParams pladmm
#' @param weights not yet implemented.
#' @param offset unused.
#' @param ... additional arguments passed to \code{pladmm}.
#' @param estfun logical. If \code{TRUE} the empirical estimating functions
#' (score/gradient contributions) are returned.
#' @param object logical. If \code{TRUE} the fitted model is returned.
#' @return a list with elements
#' \item{coefficients}{ model coefficients. }
#' \item{objfun}{ the negative log-likelihood. }
#' \item{estfun}{ if \code{estfun} the empirical estimating functions. }
#' \item{object}{ if \code{object} the fitted model. }
#' @keywords internal
#' @examples
#'
#' if (require(prefmod)){
#'   data(salad)
#'   # data.frame of rankings for salad dressings A B C D
#'   # 1 = most tart, 4 = least tart
#'   salad[1:3,]
#'
#'   # create data frame of corresponding features
#'   # (acetic and gluconic acid concentrations in salad dressings)
#'   features <- data.frame(salad = LETTERS[1:4],
#'                          acetic = c(0.5, 0.5, 1, 0),
#'                          gluconic = c(0, 10, 0, 10))
#'
#'   # create a grouped rankings object, grouping the rankings into 2 groups
#'   G <- group(as.rankings(salad), rep(1:2, nrow(salad)/2))
#'
#'   # pladmmfit() gives the same results as pladmm()
#'   mod1 <- pladmmfit(G, model.matrix(~ acetic + gluconic, data = features),
#'                     rho = 8)
#'   mod1$coefficients
#'   -mod1$objfun
#'
#'   mod2 <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)
#'   coef(mod2)
#'   logLik(mod2)
#'
#' }
#' @export
pladmmfit <- function (y, x = NULL, start = NULL, weights = NULL,
                       offset = NULL, ..., estfun = FALSE, object = FALSE) {
    if (!is.null(offset))
        warning("`offset` argument ignored")
    if (!is.null(weights))
        warning("`weights` not yet implemented")
    res <- pladmm(y, ~ 0 + x, start = start, ...)

    if (estfun) {
        percomp <- estfun.PLADMM(res)
        estfun <- rowsum(as.matrix(percomp), attr(y, "index"))
    } else estfun <- NULL
    list(coefficients = coef(res),
         objfun = -logLik(res),
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

