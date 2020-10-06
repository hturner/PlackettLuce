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
#' @param ref an integer or character string specifying the reference item (for
#' which log ability will be set to zero). If \code{NULL} the first item is
#' used.
#' @param network the network of rankings on which to base the model:
#' \code{"adaptive"} (default: rankings plus pseudodata if network not strongly
#' connected),
#' \code{"pseudodata"} (rankings plus pseudodata),
#' \code{"connected"} (rankings if network strongly connected, fail otherwise),
#' \code{"cluster"} (the largest strongly connected cluster).
#' @param npseudo when using pseudodata: the number of wins and losses to add
#' between each object and a hypothetical reference object.
#' @param epsilon the maximum absolute difference between the observed and
#' expected sufficient statistics for the ability parameters at convergence.
#' @param steffensen a threshold defined as for \code{epsilon} after which to
#' apply Steffensen acceleration to the iterative scaling updates.
#' @param maxit the maximum number of iterations.
#' @param trace logical, if \code{TRUE} show trace of iterations.
#' @param verbose logical, if \code{TRUE} show messages from validity checks.
#'
#' @return An object of class "PlackettLuce0", which is a list containing the
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
#' mod <- PlackettLuce0(R)
#' coef(mod)
PlackettLuce0 <- function(rankings, ref = NULL,
                          network = c("adaptive", "pseudodata", "connected",
                                      "cluster"),
                          npseudo = 1,
                          epsilon = 1e-7, steffensen = 1e-4, maxit = 100,
                          trace = FALSE, verbose = TRUE){
    call <- match.call()
    network <- match.arg(network, c("adaptive", "pseudodata", "connected",
                                    "cluster"))

    # check rankings
    if (!inherits(rankings, "rankings")){
        rankings <- suppressWarnings(as.rankings(rankings, verbose = verbose))
    }
    items <- colnames(rankings)
    if (is.null(attr(rankings, "no"))) attr(rankings, "no") <- 1
    # if pseudodata or (adaptive and disconnected) add pseudodata
    if (network == "pseudodata" ||
        (network == "adaptive" & attr(rankings, "no") > 1)){
        nobj <- ncol(rankings)
        rankings <- cbind("NULL" = 0, rankings)
        pseudo <- matrix(0, nrow = 2*npseudo*nobj,
                         ncol = ncol(rankings))
        pseudo[, 1] <- 1:2
        pseudo[cbind(seq_len(nrow(pseudo)),
                     rep(seq_len(nobj) + 1, each = 2))] <- 2:1
        rankings <- as.rankings(rbind(pseudo, rankings))
        pseudo <- TRUE
    } else pseudo <- FALSE
    # if disconnected, fit model to largest cluster
    if (!is.null(attr(rankings, "no")) && attr(rankings, "no") > 1){
        if (network == "connected")
            stop("Network is not strongly connected.")
        id <- which.max(attr(rankings, "csize"))
        size <- attr(rankings, "csize")[id]
        if (size == 1) {
            stop("All items are weakly connected, ",
                 "cannot estimate item abilities.")
        } else{
            warning("The network of items is split into weakly connected ",
                    "clusters\nAnalysing the largest cluster, with ", size,
                    " items")
            # drop items not in largest cluster and recode
            rankings <- rankings[, attr(rankings, "membership") == id]
            rankings <- suppressMessages(checkDense(rankings))
            if (!is.null(ref)){
                if ((is.character(ref) && !ref %in% colnames(rankings)) ||
                    !ref %in% id){
                    warning("re-setting `ref` to first object in largest ",
                            "cluster")
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

    # remove singletons - is this needed now rankings are checked?
    singleton <- J == 0
    M <- M[, !singleton, drop = FALSE]
    J <- J[!singleton]

    # sizes of selected sets (not particularly efficient - could all be == 1!)
    S <- M
    S[t(t(M) > J)] <- 0
    # S is copied onto T here to be used by loglik
    T <- S
    S@x <- 1/as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))

    # sufficient statistics

    # for alpha i, sum over all sets of object i is in selected set/size of
    # selected set
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
    pattern <- uniquecol(do.call("cbind", pattern), rep = FALSE)
    rep <- c(rowsum(rep, attr(pattern, "ind")))
    S <- length(rep)

    # starting values
    N <- ncol(rankings)
    ## (scaled, un-damped) PageRank based on underlying paired comparisons
    X <- igraph::as_adj(igraph::graph_from_edgelist(as.edgelist(rankings)))[
        colnames(rankings), colnames(rankings)]
    alpha <- drop(abs(RSpectra::eigs(X/colSums(X), 1,
                           opts = list(ncv = min(nrow(X), 10)))$vectors))
    if (pseudo) {
        alpha <- alpha/alpha[1]
    } else alpha/sum(alpha)
    delta <- rep.int(0.1, D)
    delta[1] <- 1
    #delta <- c(1, (2*B[2])/(sum(rep) - B[2]))

    # iterative scaling

    # count possible choices from sets up to size D
    count <- function(pattern, rep, D){
        # set sizes and frequencies
        freq <- rowsum(rep, colSums(pattern))
        size <- as.numeric(rownames(freq))
        # number of possible selections overall
        sum(vapply(size, choose, numeric(D), k = seq(D)) %*% freq)
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
        c(A/alpha -
              expectation0("alpha", alpha, delta, pattern, rep, N, D, S)/alpha,
          B/delta - expectation0("delta", alpha, delta, pattern, rep, N, D, S))
    }

    # Alternative optimization via
    obj <- function(par) {
        al <- exp(par[1:N])
        de <- c(1, exp(par[-c(1:N)]))
        -loglik(c(al, de))
    }
    gr <- function(par) {
        al <- exp(par[1:N])
        de <- c(1, exp(par[-c(1:N)]))
        -score(c(al, de))[-(N + 1)] * c(al, de[-1])
    }
    #res <- optim(log(c(alpha, delta[-1])), obj, gr, method = "BFGS")
    #res2 <- lbfgs(obj, gr, log(c(alpha, delta[-1])))
    conv <- FALSE
    par <- list(alpha = alpha, delta = delta)
    oneUpdate <- function(par, trace = FALSE){
        # update all alphas
        expA <- expectation0("alpha", par$alpha, par$delta, pattern, rep,
                             N, D, S, trace = trace)
        if (pseudo){
            par$alpha[-1] <- par$alpha[-1]*A[-1]/expA[-1]
        } else {
            par$alpha <- par$alpha*A/expA
        }
        # update all deltas
        if (D > 1) par$delta[-1] <- B[-1]/
                expectation0("delta", par$alpha, par$delta,
                            pattern, rep, N, D, S,
                            trace = trace)[-1]
        par
    }
    accelerate <- function(p, p1, p2){
        p - (p1 - p)^2 / (p2 - 2 * p1 + p)
    }
    eps <- 1
    doSteffensen <- FALSE
    for (iter in seq_len(maxit)){
        par <- oneUpdate(par, trace = trace)
        # steffensen
        if (doSteffensen){
            par1 <- oneUpdate(par)
            par2 <- oneUpdate(par1)
            if (pseudo){
                par$alpha[-1] <-
                    accelerate(par$alpha, par1$alpha, par2$alpha)[-1]
            } else par$alpha <- accelerate(par$alpha, par1$alpha, par2$alpha)
            par$delta[-1] <- accelerate(par$delta, par1$delta, par2$delta)[-1]
        }
        expA <- expectation0("alpha", par$alpha, par$delta, pattern, rep,
                             N, D, S)
        eps <- abs(A - expA)
        # trace
        if (trace){
            message("iter ", iter)
        }
        # stopping rule: compare observed & expected sufficient stats for alphas
        if (all(eps < steffensen & !doSteffensen)) doSteffensen <- TRUE
        if (all(eps < epsilon)) {
            conv <- TRUE
            break
        }
    }

    par$delta <- structure(par$delta, names = paste0("tie", 1:D))

    if (pseudo) {
        # drop hypothetical object
        par$alpha <- par$alpha[-1]
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
    par$alpha <- par$alpha/sum(par$alpha)
    rank <- N + D - 2

    key_q <- key_quantities(unlist(par))
    logl <- loglik(unlist(par), fit = key_q)

    if (length(par$alpha) < length(items)){
        out <- rep.int(NA_real_, length(items))
        names(out) <- items
        out[colnames(rankings)] <- par$alpha
        par$alpha <- out
    } else names(par$alpha) <- items

    if (!conv)
        warning("Iterations have not converged.")
    fit <- list(call = call,
                coefficients = c(par$alpha, par$delta[-1]),
                ref = ref,
                loglik = unname(logl),
                df.residual = count(pattern, rep, D) - rank,
                rank = rank,
                iter = iter,
                rankings = rankings,
                maxTied = D, ##  Maybe we'll want to include these differently?
                patterns = pattern, ##  Useful for fitted values
                constants = key_q$normalising_constants)
    class(fit) <- "PlackettLuce0"
    fit
}

# compute expectations of the sufficient statistics of the alphas/deltas
#' @export
expectation0 <- function(par, alpha, delta, pattern, rep = 1, N = length(alpha),
                         D = length(delta), S = ncol(pattern), trace = FALSE){
    n <- switch(par,
                "alpha" = N,
                "delta" = D)
    if (trace) message("par: ", par)
    res <- numeric(n)
    for (k in seq_len(S)){
        # objects in set
        w <- which(pattern[, k])
        nobj <- length(w)
        # single winner (tie with d = 1)
        y1 <- alpha[w]
        z1 <- sum(y1)
        # ties of order d > 1
        if (par == "delta") y1 <- numeric(n)
        d <- min(D, nobj)
        if (d > 1){
            # index up to d items: start with 1:n
            i <- seq_len(d)
            # id = index to chnage next; id2 = first index changed
            if (d == nobj) {
                id <- nobj - 1
            } else id <- d
            id2 <- 1
            repeat{
                # work along index vector from 1 to end/first index = nobj
                x1 <- alpha[w][i[1]]
                last <- i[id] == nobj
                if (last) {
                    end <- id
                } else end <- min(d, id + 1)
                for (j in 2:end){
                    # product of first j alphas indexed by i
                    x1 <- (x1 * alpha[w][i[j]])
                    # ignore if already recorded
                    if (j < id2) next
                    if (trace) message("i: ", paste(w[i], collapse = ", "))
                    # add to relevant numerator/denominator
                    if (par == "alpha"){
                        x2 <- delta[j]*x1^(1/j)
                        # add to numerators for all objects in set
                        y1[i[1:j]] <- y1[i[1:j]] + x2/j
                        if (trace)
                            message("x2/j: ", paste(round(x2/j, 7),
                                                    collapse = ", "))
                        # add to denominator for current set
                        z1 <- z1 + x2
                        if (trace)
                            message("x2: ", x2)
                    } else{
                        x2 <- x1^(1/j)
                        # add to numerator for current order
                        if (par == "delta")
                            y1[j] <- y1[j] + x2
                        # add to denominator for current set
                        z1 <- z1 + delta[j]*x2
                    }
                }
                # update index
                if (i[1] == (nobj - 1)) break
                if (last){
                    id2 <- id - 1
                    v <- i[id2]
                    len <- min(nobj - 2 - v, d - id2)
                    id <- id2 + len
                    i[id2:id] <- v + seq_len(len + 1)
                } else {
                    id2 <- id
                    i[id] <- i[id] + 1
                }
            }
        }
        if (trace){
            message("items: ", paste(w, collapse = ", "))
            message("y1: ", paste(round(y1, 7), collapse = ", "))
            message("z1: ", z1)
        }
        # add contribution for set to expectation
        if (par == "alpha"){
            if (trace) {
                message("y1/z1:", paste(round(y1/z1, 7), collapse = ", "))
                message("rep[k]:", paste(round(rep[k]), collapse = ", "))
                message("add to:", round(res[w], 7))
            }
            res[w] <- res[w] + rep[k]*y1/z1
        } else res <- res + rep[k]*y1/z1
    }
    res
}
