#' Fit Plackett-Luce Model
#'
#' Fit a Plackett-Luce model to a set of rankings. The rankings may be partial
#' (not all objects ranked) and include ties of any order.
#'
#' @param rankings a matrix of dense rankings, see examples.
#' @param ref an integer or character string specifying the reference item (for which log ability will be set to zero). If \code{NULL} the first item is used.
#' @param epsilon the maximum absolute difference between the observed and
#' expected sufficient statistics for the ability parameters.
#' @param maxit the maximum number of iterations.
#' @param trace logical, if \code{TRUE} show trace of iterations.
#'
#' @return An object of class "PlackettLuce", which is a list containing the
#' following elements:
#' \item{call}{ The matched call. }
#' \item{coefficients}{ The model coefficients. }
#' \item{loglik}{ The log-likelihood. }
#' \item{iter}{ The number of iterations run. }
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
PlackettLuce <- function(rankings, ref = NULL, epsilon = 1e-7, maxit = 100,
                         trace = FALSE){
    call <- match.call()

    # check rankings
    if (!inherits(rankings, "rankings")){
        rankings <- as.rankings(rankings)
    }
    # if disconnected, fit model to largest cluster
    if (attr(rankings, "no") > 1){
        id <- which.max(attr(rankings, "csize"))
        size <- attr(rankings, "csize")[id]
        if (size == 1) {
            stop("All items are weakly connected, cannot estimate item abilities.")
        } else{
            warning("The network of items is split into weakly connected clusters\n",
                    "Analysing the largest cluster, with ", size, " items")
            # drop items not in largest cluster and recode
            rankings <- rankings[, attr(rankings, "membership") == id]
            rankings <- suppressWarnings(checkDense(rankings))
        }
    }

    M <- t(Matrix(unclass(rankings), sparse = TRUE))

    # max nsets
    J <- apply(M, 2, max)

    # highest degree of ties
    # (specific to dense ranking - need to check and convert other rankings)
    n <- diff(M@p)
    ties0 <- n - J + 1
    D <- max(ties0)

    # nontrivial nsets (excluding untied last place)
    J <- J - as.numeric(rowSums(t(M) == J) == 1)
    Jmax <- max(J)

    # remove singletons
    singleton <- J == 0
    M <- M[, !singleton]
    J <- J[!singleton]
    ties0 <- ties0[!singleton]

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
    N <- ncol(rankings)
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

    # count possible choices from sets up to size D
    count <- function(pattern, rep, D){
        # set sizes and frequencies
        freq <- rowsum(rep, colSums(pattern))
        size <- as.numeric(rownames(freq))
        # number of possible selections overall
        sum(sapply(size, choose, seq(D)) %*% freq)
    }

    key_quantities <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)]
        cs <- numeric(S)
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
            cs[k] <- z1
        }
        list(alpha = alpha, delta = delta, normalising_constants = cs)
    }

    # Design loglik as brglm2::brglmFit
    # log-likelihood and score functions
    # Within optim or nlminb use obj and gr wrappers below
    loglik <- function(par, fit = NULL) {
        if (is.null(fit)) {
            fit <- key_quantities(par)
        }
        alpha <- fit$alpha
        delta <- fit$delta
        c_contr <- sum(log(fit$normalising_constants) * rep)
        b_contr <- 0
        ## nominators
        for(k in seq_len(Jmax)) {
            w <- J >= k
            x <- sum(log(delta[ties0[w]]) + apply((T == k)[, w, drop = FALSE], 2, function(z) sum(log(alpha[z])))/ties0[w])
            b_contr <- b_contr + x
        }
        b_contr - c_contr
    }

    # log-likelihood derivatives
    score <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)]
        c(A/alpha - expectation("alpha")/alpha,
          B/delta - expectation("beta"))
    }

    # Alternative optimization via
    ## obj <- function(par) {
    ##     al <- exp(par[1:N])
    ##     de <- c(1, exp(par[-c(1:N)]))
    ##     -loglik(c(al, de))
    ## }
    ## gr <- function(par) {
    ##     al <- exp(par[1:N])
    ##     de <- c(1, exp(par[-c(1:N)]))
    ##     -score(c(al, de))[-(N + 1)] * c(al, de[-1])
    ## }
    ## res <- optim(log(c(alpha, delta[-1])), obj, gr, method = "BFGS")
    for (iter in seq_len(maxit)){
        # update all alphas
        expA <- expectation("alpha")
        alpha <- alpha*A/expA
        # scale alphas to sum 1
        alpha <- alpha/sum(alpha)
        # update all deltas
        if (D > 1) delta[-1] <- B[-1]/expectation("beta")[-1]
        # trace
        if (trace){
            message("iter ", iter)
        }
        # stopping rule: compare observed & expected sufficient stats for alphas
        if (all(abs(A - expA) < epsilon)) break
    }

    if (is.null(names(alpha))) names(alpha) <- paste0("alpha", seq_along(alpha))
    delta <- structure(delta, names = paste0("tie", 1:D))
    rank <- N + D + sum(rep) - 2

    key_q <- key_quantities(c(alpha, delta))
    logl <- loglik(c(alpha, delta), fit = key_q)

    fit <- list(call = call,
                coefficients = c(alpha, delta[-1]),
                ref = if (is.null(ref)) 1 else ref,
                loglik = unname(logl),
                df.residual = count(pattern, rep, D) - rank,
                rank = rank,
                iter = iter,
                rankings = rankings,
                maxTied = D, ##  Maybe we'll want to include these differently?
                patterns = pattern, ##  Useful for fitted values
                constants = key_q$normalising_constants)
    class(fit) <- "PlackettLuce"
    fit
}
