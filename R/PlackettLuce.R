#' Fit Plackett-Luce Model
#'
#' Fit a Plackett-Luce model to a set of rankings. The rankings may be partial
#' (not all objects ranked) and include ties of any order.
#'
#' @param rankings a matrix of dense rankings, see examples.
#' @param maxit the maximum number of iterations.
#' @param trace logical, if \code{TRUE} show trace of iterations.
#'
#' @return A list with the following elements:
#' \item{coefficients}{The model coefficients.}
#' \item{iter}{The number of iterations run.}
#'
#' @examples
#' # Six partial rankings of four objects, 1 is top rank, e.g
#' # first ranking: item 1, item 2
#' # second ranking: item 2, item 3, item 4, item 1
#' # third ranking: items 2, 3, 4 tie for first place, item 1 second
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#'
#' mod <- PlackettLuce(R)
#' coef(mod)
#' @import Matrix
#' @export
PlackettLuce <- function(rankings, maxit = 100, trace = FALSE){
    N <- ncol(rankings)
    M <- t(Matrix(rankings, sparse = TRUE))

    # max nsets
    J <- apply(M, 2, max)

    # highest degree of ties
    # (specific to dense ranking - need to check and convert other rankings)
    n <- diff(M@p)
    ties0 <- n - J + 1
    D <- max(ties0)

    # nontrivial nsets (excluding untied last place)
    J <- J - as.numeric(rowSums(t(M) == J) == 1)

    # sizes of selected sets (not particularly efficient - could all be == 1!)
    S <- M
    S[t(t(M) > J)] <- 0
    # S is copied onto T here to be used by loglik
    T <- S
    S@x <- 1/as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))

    # sufficient statistics

    # for alpha i, sum over all sets of object i is in selected set/size of selected set
    A <- rowSums(S)
    # for delta d, number of sets with cardinality d
    B <- tabulate(1/S@x)/(1:D)

    # unique columns with >= 1 element for logical matrix
    uniquecol <- function(M, rep = TRUE){
        min2 <- colSums(M) > 1
        if (!any(min2)) return(NULL)
        for (i in seq_len(nrow(M))){
            if (i == 1) {
                pattern <- M[1,]
            }
            else {
                pattern <- 2*pattern + M[i,]
                pattern <- match(pattern, unique(pattern))
            }
        }
        uniq <- !duplicated(pattern) & min2
        ind <- match(pattern, pattern[uniq])
        if (rep){
            rep <- tabulate(ind)
            structure(M[, uniq, drop = FALSE], rep = rep)
        } else structure(M[, uniq, drop = FALSE], ind = ind)
    }

    # get unique sets and reps
    pattern <- list()
    for (j in seq_len(max(J))){
        pattern[[j]] <- uniquecol(M >= j)
    }
    rep <- unlist(lapply(pattern, attr, "rep"))
    pattern <- uniquecol(do.call("cBind", pattern), rep = FALSE)
    rep <- c(rowsum(rep, attr(pattern, "ind")))
    S <- length(rep)

    # starting values
    alpha <- rep.int(1/N, N)
    delta <- rep.int(0.1, D)
    delta[1] <- 1
    #delta <- c(1, (2*B[2])/(sum(rep) - B[2]))

    # iterative scaling
    expectation <- function(par){
        n <- switch(par,
                    "alpha" = N,
                    "beta" = D)
        res <- numeric(n)
        for (k in seq_len(S)){
            # objects in set
            w <- which(pattern[, k])
            nobj <- length(w)
            # ties of order d
            y1 <- numeric(n)
            z1 <- 0
            for (d in seq_len(min(D, nobj))){
                i <- seq_len(d)
                maxi <- rev(nobj - seq_len(d) + 1)
                # loop through all subsets of size d in set
                if (par == "alpha")
                    y2 <- numeric(n)
                z2 <- 0
                repeat{
                    x <- prod(alpha[w[i]])^(1/d)
                    # add to sums for all objects in set
                    if (par == "alpha")
                        y2[w[i]] <- y2[w[i]] + x
                    # add to sum for current order
                    if (par == "beta")
                        y1[d] <- y1[d] + x
                    # add to sum for current set
                    z2 <- z2 + x
                    if (i[1] == maxi[1]) break
                    id <- max(which(maxi - i > 0))
                    i[id:d] <- i[id] + seq_len(d - id + 1)
                }
                if (par == "alpha")
                    y1 <- y1 + delta[d]/d*y2
                z1 <- z1 + delta[d]*z2
            }
            res <- res + rep[k]*y1/z1
        }
        res
    }

    # log-likelihood and score functions
    # Within optim or nlminb use obj and gr wrappers below
    loglik <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)]
        c_contr <- b_contr <- 0
        ## denominators
        for (k in seq_len(S)) {
            w <- which(pattern[, k])
            nobj <- length(w)
            ## ties of order d
            z1 <- 0
            for (d in seq_len(min(D, nobj))){
                i <- seq_len(d)
                maxi <- rev(nobj - seq_len(d) + 1)
                # loop through all subsets of size d in set
                z2 <- 0
                repeat{
                    x <- prod(alpha[w[i]])^(1/d)
                    z2 <- z2 + x
                    if (i[1] == maxi[1]) break
                    id <- max(which(maxi - i > 0))
                    i[id:d] <- i[id] + seq_len(d - id + 1)
                }
                z1 <- z1 + delta[d]*z2
            }
            c_contr <- c_contr + log(z1) * rep[k]
        }
        ## nominators
        for(k in seq_len(D)) {
            w <- J >= k
            x <- sum(log(delta[ties0[w]]) + apply((T == k)[, w, drop = FALSE], 2, function(z) sum(log(alpha[z])))/ties0[w])
            b_contr <- b_contr + x
        }
        b_contr - c_contr
    }

    # Log-likelihood derivatives
    score <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)]
        c(A/alpha - expectation("alpha")/alpha,
          B/delta - expectation("beta"))
    }

    ## # Alternative optimization via
    ## obj <- function(par) {
    ##     al <- exp(par[1:N])
    ##     de <- c(1, exp(par[-c(1:N)]))
    ##     -loglik(c(al, de))
    ## }
    ## res <- optim(log(c(alpha, delta[-1])), obj, method = "BFGS")

    for (iter in seq_len(maxit)){
        # update all alphas
        old <- alpha
        alpha <- alpha*A/expectation("alpha")
        # scale alphas to sum 1
        alpha <- alpha/sum(alpha)
        # update all deltas
        if (D > 1) delta[-1] <- B[-1]/expectation("beta")[-1]
        # trace
        if (trace){
            message("iter ", iter)
        }
        # simple stopping rule just based on alphas for now
        if (isTRUE(all.equal(log(old), log(alpha)))) break
    }

    if (is.null(names(alpha))) names(alpha) <- paste0("alpha", seq_along(alpha))
    delta <- structure(delta, names = paste0("tie", 1:D))
    list(coefficients = c(alpha, delta[-1]),
         iter = iter,
         loglik = loglik(c(alpha, delta)))
}
