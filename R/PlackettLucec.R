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
#' @param method  the method to be used for fitting: \code{"iterative scaling"} (default: iterative scaling to sequentially update the parameter values), \code{"BFGS"} (the BFGS optimisation algorithm through the \code{\link{optim}} interface), \code{"L-BFGS"} (the limited-memory BFGS optimisation algorithm as implemented in the \code{\link{lbfgs}} package).
#' @param epsilon the maximum absolute difference between the observed and
#' expected sufficient statistics for the ability parameters at convergence.
#' @param steffensen a threshold defined as for \code{epsilon} after which to
#' apply Steffensen acceleration to the iterative scaling updates.
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
#' @import Matrix lbfgs data.table
#' @importFrom igraph as_adj graph_from_edgelist
#' @importFrom rARPACK eigs
#' @export
PlackettLuce <- function(rankings, ref = NULL,
                         network = c("adaptive", "pseudodata", "connected",
                                     "cluster"),
                         npseudo = 1,
                         method = c("iterative scaling", "BFGS", "L-BFGS"),
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

    # sparse matrix of dense rankings
    # rankings object should probably be sparse, but some methods may need update
    R <- Matrix(unclass(rankings), sparse = TRUE)
    rm(rankings)

    # nontrivial nsets (excluding untied last place)
    J <- apply(R, 1, max)
    J <- J - as.numeric(rowSums(R == J) == 1)

    # sizes of selected sets (not particularly efficient - could all be == 1!)
    S <- R
    S[which(R > J)] <- 0
    S <- t(S)
    S@x <- as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))

    # sufficient statistics

    # for alpha i, sum over all sets st object i is in selected set/size of selected set
    A <- unname(rowsum(1/S@x, S@i)[,1])
    # for delta d, number of sets with cardinality d/cardinality
    B <- tabulate(S@x)
    D <- length(B)
    B <- B/seq(D)

    # starting values - as.edgelist.rankings doesn't currently work for Matrix

    # (scaled, un-damped) PageRank based on underlying paired comparisons
    if (is.null(colnames(R))) colnames(R) <- 1:ncol(R)
    X <- as_adj(graph_from_edgelist(as.edgelist.rankings(as.matrix(R))))[colnames(R),
                                                                         colnames(R)]
    alpha <- drop(abs(eigs(X/colSums(X), 1,
                           opts = list(ncv = min(nrow(X), 10)))$vectors))
    # alpha <- alpha/sum(alpha) current version doesnt divide by alpha
    delta <- rep.int(0.1, D)
    delta[1] <- 1

    # pre-process rankings so easier to compute on

    # unique rows with >= 1 element for logical matrix
    uniquerow <- function(M, rep = TRUE){
        for (i in seq_len(ncol(M))){
            if (i == 1) {
                pattern <- M[,1]
            }
            else {
                pattern <- 2*pattern + M[,i]
                pattern <- match(pattern, unique(pattern))
            }
        }
        pattern
    }


    # total number of objects
    N <- ncol(R)

    # max number of objects in set
    nc <- max(rowSums(R > 0))

    # number of rankings
    nr <- nrow(R)

    # create S so cols are potential set sizes and value > 0 indicates ranking
    # aggregate common sets
    # - incorporate ranking weights by weighted count vs simple tabulate
    S <- as(sparseMatrix(dims = c(nr, nc), i = {}, j = {}), "dgCMatrix")
    T <- sparseMatrix(dims = c(nr, N), i = {}, j = {})
    P <- logical(nc)  # set sizes present in rankings
    minrank <- rep.int(1, nr)
    for (i in seq_len(nc)){
        p <- (nc - i + 1)
        pattern <- drop0(R >= minrank)
        size <- rowSums(pattern)
        r <- size == p
        tied <- !r & minrank != 1
        if (any(tied)) T[tied, p] <- TRUE
        P[p] <- p != 1 && any(r)
        if (!P[p] ) next
        g <- uniquerow(pattern[r, , drop = FALSE])
        ng <- tabulate(g)
        S[r, , drop = FALSE][!duplicated(g), p] <- ng
        minrank[r] <- minrank[r] + 1
    }
    P <- which(P)

    # multiply by rankings weights here

    # replace rankings with items ordered by ranking (all that is needed now)
    R <- as(t(R), "dgTMatrix")
    R@x <- R@i[do.call(order, list(R@j, R@x, decreasing = c(FALSE, TRUE),
                                   method = "radix"))] + 1
    R@i <- sequence(colSums(R > 0)) - 1L
    R <- t(R)

    # quasi-newton methods ---

    key_quantities <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)] # includes delta1
        res <- expectation2("all", alpha, c(1, delta), R, S, N, D, P)
        # sum of (log(normalising_constants_per_set) * rep)
        c_contr <- sum(res$theta)
        list(alpha = alpha, delta = delta, c_contr = c_contr,
             expA = res$expA, expB = res$expB)
    }

    # Design loglik as brglm2::brglmFit
    # log-likelihood and score functions
    # Within optim or nlminb use obj and gr wrappers below
    #
    # assign key quantities to function environment to re-use
    loglik <- function(par) {
        assign("fit", key_quantities(par), envir = parent.env(environment()))
        b_contr <- sum(B[-1]*log(fit$delta)) + sum(A*log(fit$alpha))
        b_contr - fit$c_contr
    }

    # log-likelihood derivatives
    score <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)]
        c(A/alpha - fit$expA/alpha,
          B[-1]/delta - fit$expB)
    }

    # Alternative optimization via
    obj <- function(par) {
        al <- exp(par[1:N])
        de <- exp(par[-c(1:N)])
        -loglik(c(al, de))
    }
    gr <- function(par) {
        al <- exp(par[1:N])
        de <- exp(par[-c(1:N)])
        -score(c(al, de)) * c(al, de)
    }

    method <- match.arg(method, c("iterative scaling", "BFGS", "L-BFGS"))

    if (method == "BFGS"){
        res <- optim(log(c(alpha, delta[-1])), obj, gr, method = "BFGS")
        conv <- res$convergence == 0
        iter <- res$counts
        res <- list(alpha = exp(res$par[1:N]),
                    delta = c(1, exp(res$par[-(1:N)])))

    } else if (method == "L-BFGS"){
        ok <- requireNamespace("lbfgs", quietly = TRUE)
        if (!ok) stop('`method = "lbfgs"` requires lbfgs package')
        res <- lbfgs(obj, gr, log(c(alpha, delta[-1])), invisible = 1)
        conv <- res$convergence == 0
        iter <- NULL
        res <- list(alpha = exp(res$par[1:N]),
                    delta = c(1, exp(res$par[-(1:N)])))
    } else {
        res <- list(alpha = alpha, delta = delta)
        res[c("expA", "expB")] <-
            expectation2("all", alpha, delta,  R, S, N, D, P)[c("expA", "expB")]
        oneUpdate <- function(res){
            # update all alphas
            if (pseudo){
                res$alpha[-1] <- res$alpha[-1]*A[-1]/res$expA[-1]
            } else {
                res$alpha <- res$alpha*A/res$expA
            }
            # update all deltas
            if (D > 1) res$delta[-1] <- B[-1]/
                    expectation2("delta", res$alpha, res$delta, R, S, N, D, P)$expB
            res[c("expA", "expB")] <-
                expectation2("all", res$alpha, res$delta, R, S, N, D, P)[c("expA", "expB")]
            res
        }
        accelerate <- function(p, p1, p2){
            # only accelerate if parameter has changed in last iteration
            d <- p2 != p1
            p2[d] <- p[d] - (p1[d] - p[d])^2 / (p2[d] - 2 * p1[d] + p[d])
            p2
        }
        # stopping rule: compare observed & expected sufficient stats
        checkConv <- function(res){
            assign("eps", abs(c(A, B[-1]) - c(res$expA, res$delta[-1]*res$expB)),
                   envir = parent.env(environment()))
            all(eps < epsilon)
        }
        if (conv <- checkConv(res)) {
            maxit <- iter <- 0
        } else iter <- 1
        updateIter <- function(){
            if (trace) message("iter: ", iter)
            assign("iter", iter + 1,
                   envir = parent.env(environment()))
        }
        doSteffensen <- FALSE
        while(iter <= maxit){
            res <- oneUpdate(res)
            if (conv <- checkConv(res)) break
            if (all(eps < steffensen & !doSteffensen)) doSteffensen <- TRUE
            # steffensen
            if (doSteffensen){
                res1 <- oneUpdate(res)
                if (conv <- checkConv(res1)) break
                res2 <- oneUpdate(res1)
                if (conv <- checkConv(res2)) break
                updateIter()
                if (pseudo){
                    res$alpha[-1] <- accelerate(res$alpha, res1$alpha, res2$alpha)[-1]
                } else res$alpha <- accelerate(res$alpha, res1$alpha, res2$alpha)
                res$delta[-1] <- accelerate(res$delta, res1$delta, res2$delta)[-1]
                res[c("expA", "expB")] <-
                    expectation2("all", res$alpha, res$delta,
                                 R, S, N, D, P)[c("expA", "expB")]
                if (conv <- checkConv(res)) break
            } else{
                updateIter()
            }
        }
    }
    res$delta <- structure(res$delta, names = paste0("tie", 1:D))[-1]

    if (pseudo) { ## needs fixing up, based on od patterns approach
        # drop hypothetical object
        res$alpha <- res$alpha[-1]
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
    res$alpha <- res$alpha/sum(res$alpha)
    rank <- N + D + sum(S[, P]) - 2

    # count possible choices from sets up to size D
    count <- function(S, P, D){
        # frequencies of sets selected from, for sizes 2 to max observed
        freq <- colSums(S[, P])
        # number of possible selections overall
        sum(sapply(P, choose, seq(D)) %*% freq)
    }
    df.residuals <- count(S, P, D) - rank

    logl <- loglik(unlist(res[c("alpha", "delta")]))

    # recreate rankings to return
    S <- as(R > 0, "dgCMatrix")
    S@x[which(T[which(R > 0)])] <- 0
    S <- t(S)
    S@x <- unlist(unname(tapply(S@x, R@i,
                                function(x) rev(cumsum(rev(x))))))
    R <- sparseMatrix(x = S@x, j = R@x, i = R@i + 1)

    if (length(res$alpha) < length(items)){
        out <- rep.int(NA_real_, length(items))
        names(out) <- items
        out[colnames(R)] <- res$alpha
        res$alpha <- out
    } else names(res$alpha) <- items

    if (!conv)
        warning("Iterations have not converged.")

    fit <- list(call = call,
                coefficients = c(res$alpha, res$delta),
                ref = ref,
                loglik = unname(logl),
                df.residual = df.residual,
                rank = rank,
                iter = iter,
                rankings = R,
                maxTied = D#, ##  Maybe we'll want to include these differently?
                #constants = key_q$normalising_constants
                )
    class(fit) <- "PlackettLuce"
    fit
}

# function to compute expectations of the sufficient statistics of the alphas/deltas
expectation2c <-  function(par, alpha, delta, R, S, N, D, P){
    keepAlpha <- par %in% c("alpha", "all")
    keepDelta <- par %in% c("delta", "all") & D > 1
    keepTheta <- par %in% c("theta", "all")
    expA <- expB <- theta <- NULL
    if (keepAlpha) expA <- numeric(N)
    if (keepDelta) expB <- numeric(D - 1)
    if (keepTheta) theta <- numeric(sum(lengths(S[P])))
    z <- 1
    for (p in P){
        # D == 1
        ## numerators (for expA, else just to compute denominators)
        r <- attr(S, "ind")[[p]]
        nr <- length(r)
        x1 <- matrix(alpha[unlist(sapply(R[r], "[", 1:p, simplify = FALSE))],
                     nrow = nr, ncol = p)
        ## denominators
        z1 <- rowSums(x1)
        # D > 1
        d <- min(D, p)
        if (d > 1){
            if (keepDelta)
                y1 <- matrix(0, nrow = nr, ncol = D - 1)
            # index up to d items: start with 1:n
            i <- seq_len(d)
            # id = index to change next; id2 = first index changed
            if (d == p) {
                id <- p - 1
            } else id <- d
            id2 <- 1
            repeat{
                # work along index vector from 1 to end/first index = p
                v1 <- alpha[vapply(R[r], "[", numeric(1), i[1])] # ability for first ranked item
                last <- i[id] == p
                if (last) {
                    end <- id
                } else end <- min(d, id + 1)
                for (k in 2:end){
                    # product of first k alphas indexed by i
                    v1 <- v1 * alpha[c(vapply(R[r], "[", numeric(1), i[k]))]
                    # ignore if already recorded
                    if (k < id2) next
                    # add to numerators/denominators for sets of order p
                    v2 <- v1^(1/k)
                    v3 <- delta[k]*v2
                    if (keepAlpha) {
                        # add to numerators for objects in sets
                        x1[, i[1:k]] <- x1[, i[1:k]] + v3/k
                    }
                    if (keepDelta) {
                        # add to numerator for current tie order for sets
                        y1[, k - 1] <- y1[, k - 1] + v2
                    }
                    # add to denominators for sets
                    z1 <- z1 + v3
                }
                # update index
                if (i[1] == (p - 1)) break
                if (last){
                    id2 <- id - 1
                    v <- i[id2]
                    len <- min(p - 2 - v, d - id2)
                    id <- id2 + len
                    i[id2:id] <- v + seq_len(len + 1)
                } else {
                    id2 <- id
                    i[id] <- i[id] + 1
                }
            }
        }
        # add contribution for sets of size p to expectation
        if (keepAlpha){
            # R[r, 1:p] may only index some alphas
            add <- drop(rowsum(as.vector(S[[p]] * x1/z1),
                               unlist(sapply(R[r], "[", 1:p, simplify = FALSE))))
            expA[as.numeric(names(add))] <- expA[as.numeric(names(add))] + add
        }
        if (keepDelta){
            expB <- expB + colSums(S[[p]] * y1/z1)
        }
        if (keepTheta){
            if (par == "all"){
                # return logtheta
                theta[z:(z + nr - 1)] <- S[[p]] * log(z1)
            } else {
                theta[z:(z + nr - 1)] <- S[[p]] * z1
            }
            z <- z + nr
        }
    }
    list(expA = expA, expB = expB, theta = theta)
}

# aggregate weights to combine sets within rankings
# - either combine sets that contain the same items ("all"),
# or sets where the winning set and the losing set are the same ("winners_and_losers")
aggregate_weights <- function(R, S, N, P, method = c("winners_and_losers", "all")){
    # create index for row patterns with >= 1 element in a logical matrix
    rowpattern <- function(M){
        for (i in seq_len(ncol(M))){
            if (i == 1) {
                pattern <- M[,1]
            }
            else {
                pattern <- 2*pattern + M[,i]
                pattern <- match(pattern, unique(pattern))
            }
        }
        pattern
    }

    # aggregate partial rankings by set size
    for (i in P){
        r <- which(S[,i] > 0)
        nr <- length(r)
        if (i == N & method == "all") { # only 1 possible group
            S[,i][S[,i] > 0] <- c(nr, numeric(nr - 1))
            next
        }
        # (one of the) item(s) in i'th from last place
        minrank <- rowSums(S[r, i:N, drop = FALSE])
        # items in final set of size i
        pattern <- R[r, , drop = FALSE] >= minrank
        # find unique sets and select representative ranking to compute on
        g <- rowpattern(pattern)
        if (method == "winners_and_losers"){
            h <- rowpattern(R[r, , drop = FALSE] == minrank)
            g <- paste(g, h, sep = ":")
            return(list(minrank = minrank, g = match(g, unique(g))))
        }
        ng <- tabulate(g)
        if (any(ng > 1)){
            dup <- duplicated(g)
            S[r[dup], i] <- 0
            S[r[!dup], i] <- ng
        }
    }
    S
}
