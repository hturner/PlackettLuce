#' Fit Plackett-Luce Model
#'
#' Fit a Plackett-Luce model to a set of rankings. The rankings may be partial
#' (not all objects ranked) and include ties of any order.
#'
#' In order for the maximum likelihood estimate of an object's ability to be
#' defined, the network of rankings must be strongly connected. This means that
#' in every possible partition of the objects into two nonempty subsets, some
#' object in the second set is ranked higher than some objects in the first set
#' at least once.
#'
#' If the network of rankings is not strongly connected, or the network is
#' sparse, then pseudodata may be used to connect the network and reduce the
#' bias in the ability estimates. This approach posits a hypothetical object
#' with log-ability 0 and adds \code{npseudo} wins and \code{npseudo} losses
#' to the set of rankings.
#'
#' The paramater \code{npseudo} is the prior strength.  With \code{npseudo = 0}
#' we have the MLE as the posterior mode.  As \code{npseudo} approaches
#' infinity the log-ability estimates all shrink towards 0. The default,
#' \code{npseudo = 1}, is sufficient to connect the network and has a weak
#' shrinkage effect.
#'
#' @param rankings a matrix of dense rankings, see examples.
#' @param ref an integer or character string specifying the reference item (for which log ability will be set to zero). If \code{NULL} the first item is used.
#' @param network the network of rankings on which to base the model:
#' \code{"adaptive"} (default: rankings plus pseudodata if network not strongly connected),
#' \code{"pseudodata"} (rankings plus pseudodata),
#' \code{"connected"} (rankings if network strongly connected, fail otherwise),
#' \code{"cluster"} (the largest strongly connected cluster).
#' @param npseudo when using pseudodata: the number of wins and losses to add
#' between each object and a hypothetical reference object.
#' @param epsilon the maximum absolute difference between the observed and
#' expected sufficient statistics for the ability parameters.
#' @param maxit the maximum number of iterations.
#' @param trace logical, if \code{TRUE} show trace of iterations.
#' @param verbose logical, if \code{TRUE} show messages from validity checks.
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
PlackettLuce <- function(rankings, ref = NULL,
                         network = c("adaptive", "pseudodata", "connected",
                                     "cluster"),
                         npseudo = 1,
                         epsilon = 1e-7, maxit = 100,
                         trace = FALSE, verbose = TRUE){
    call <- match.call()
    network <- match.arg(network, c("adaptive", "pseudodata", "connected",
                                    "cluster"))

    # check rankings
    if (!inherits(rankings, "rankings")){
        rankings <- suppressWarnings(as.rankings(rankings, verbose = verbose))
    }
    items <- colnames(rankings)
    # if pseudodata or (adaptive and disconnected) add pseudodata
    if (network == "pseudodata" ||
        (network == "adaptive" & attr(rankings, "no") > 1)){
        nobj <- ncol(rankings)
        rankings <- cbind(0, rankings)
        pseudo <- matrix(0, nrow = 2*npseudo*nobj,
                         ncol = ncol(rankings))
        pseudo[, 1] <- 1:2
        pseudo[cbind(seq_len(nrow(pseudo)),
                     rep(seq_len(nobj) + 1, each = 2))] <- 2:1
        rankings <- rbind(pseudo, rankings)
        pseudo <- TRUE
    } else pseudo <- FALSE
    # if disconnected, fit model to largest cluster
    if (!is.null(attr(rankings, "no")) && attr(rankings, "no") > 1){
        if (network == "connected")
            stop("Network is not strongly connected.")
        id <- which.max(attr(rankings, "csize"))
        size <- attr(rankings, "csize")[id]
        if (size == 1) {
            stop("All items are weakly connected, cannot estimate item abilities.")
        } else{
            warning("The network of items is split into weakly connected clusters\n",
                    "Analysing the largest cluster, with ", size, " items")
            # drop items not in largest cluster and recode
            rankings <- rankings[, attr(rankings, "membership") == id]
            rankings <- suppressMessages(checkDense(rankings))
            if (!is.null(ref)){
                if ((is.character(ref) && !ref %in% colnames(rankings)) ||
                    !ref %in% id){
                    warning("re-setting `ref` to first object in largest cluster")
                    ref <- id[1]
                }
            }
        }
    }
    if (is.null(ref)) ref <- 1

    M <- t(Matrix(unclass(rankings), sparse = TRUE))

    # max nsets
    J <- apply(M, 2, max)

    # nontrivial nsets (excluding untied last place)
    J <- J - as.numeric(rowSums(t(M) == J) == 1)
    Jmax <- max(J)

    # remove singletons
    singleton <- J == 0
    M <- M[, !singleton]
    J <- J[!singleton]

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
    B <- tabulate(1/S@x)
    D <- length(B)
    B <- B/seq(D)

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
    if (pseudo) alpha[1] <- 1
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
            x <- apply((T == k)[, w, drop = FALSE], 2, function(z){
                ties0 <- sum(z)
                sum(log(delta[ties0])) + sum(log(alpha[z]))/ties0
            })
            b_contr <- b_contr + sum(x)
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
    conv <- FALSE
    for (iter in seq_len(maxit)){
        # update all alphas
        expA <- expectation("alpha")
        if (pseudo){
            alpha[-1] <- alpha[-1]*A[-1]/expA[-1]
            # no need to scale as alpha[1] fixed
        } else {
            alpha <- alpha*A/expA
            # scale alphas to sum 1
            alpha <- alpha/sum(alpha)
        }
        # update all deltas
        if (D > 1) delta[-1] <- B[-1]/expectation("beta")[-1]
        # trace
        if (trace){
            message("iter ", iter)
        }
        # stopping rule: compare observed & expected sufficient stats for alphas
        if (all(abs(A - expA) < epsilon)) {
            conv <- TRUE
            break
        }
    }

    delta <- structure(delta, names = paste0("tie", 1:D))

    if (pseudo) {
        # drop hypothetical object
        alpha <- alpha[-1]
        N <- N - 1
        # drop extra rankings
        extra <- seq_len(2*npseudo*nobj)
        rankings <- rankings[-extra, -1]
        T <- T[-1, -extra]
        J <- J[-extra]
        # drop extra patterns
        pattern <- pattern[-1, -seq_len(nobj)]
        rep <- rep[-seq_len(nobj)]
        S <- S - nobj
    }
    rank <- N + D + sum(rep) - 2

    key_q <- key_quantities(c(alpha, delta))
    logl <- loglik(c(alpha, delta), fit = key_q)

    if (length(alpha) < length(items)){
        out <- rep.int(NA_real_, length(items))
        names(out) <- items
        out[names(alpha)] <- alpha
        alpha <- out
    }

    if (!conv)
        warning("Iterations have not converged.")
    fit <- list(call = call,
                coefficients = c(alpha, delta[-1]),
                ref = ref,
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
