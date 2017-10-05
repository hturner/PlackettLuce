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
#' The parameter \code{npseudo} is the prior strength.  With \code{npseudo = 0}
#' we have the MLE as the posterior mode.  As \code{npseudo} approaches
#' infinity the log-ability estimates all shrink towards 0. The default,
#' \code{npseudo = 1}, is sufficient to connect the network and has a weak
#' shrinkage effect.
#'
#' @param rankings a \code{"\link{rankings}"} object, or an object that can be
#' coerced by \code{as.rankings}.
#' @param ref an integer or character string specifying the reference item (for
#' which log ability will be set to zero). If \code{NULL} the first item is used.
#' @param npseudo when using pseudodata: the number of wins and losses to add
#' between each object and a hypothetical reference object.
#' @param weights an optional vector of weights for each ranking.
#' @param method  the method to be used for fitting: \code{"iterative scaling"} (default: iterative scaling to sequentially update the parameter values), \code{"BFGS"} (the BFGS optimisation algorithm through the \code{\link{optim}} interface), \code{"L-BFGS"} (the limited-memory BFGS optimisation algorithm as implemented in the \code{\link[lbfgs]{lbfgs}} package).
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
#' @importFrom igraph as_adj graph_from_edgelist
#' @importFrom rARPACK eigs
#' @importFrom stats optim
#' @export
PlackettLuce <- function(rankings, ref = NULL,
                         npseudo = 0.5,
                         weights = NULL,
                         method = c("iterative scaling", "BFGS", "L-BFGS"),
                         epsilon = 1e-7, steffensen = 1e-4, maxit = 200,
                         trace = FALSE, verbose = TRUE){
    call <- match.call()

    # check rankings
    grouped_rankings <- inherits(rankings, "grouped_rankings")
    if (grouped_rankings){
        # weights are per group id - expand to be per ranking
        if (!is.null(weights)) {
            stopifnot(length(weights) == max(attr(rankings, "index")))
            weights <- weights[attr(rankings, "index")]
        }
        R <- attr(rankings, "R")
        S <- attr(rankings, "S")
        id <- attr(rankings, "id")
        rankings <- attr(rankings, "rankings")
    } else if (!inherits(rankings, "rankings")){
        rankings <- suppressWarnings(as.rankings(rankings, verbose = verbose))
    }

    # attributes
    items <- colnames(rankings)
    N <- ncol(rankings) # total number of objects
    nr <- nrow(rankings) # number of rankings

    # weights
    if (is.null(weights)){
        weights <- rep.int(1, nr)
    } else stopifnot(length(weights) == nrow(rankings))

    if (is.null(ref)) ref <- 1

    if (!grouped_rankings){
        # items ranked from last to 1st place
        R <- t(apply(rankings, 1, order, decreasing = TRUE))

        # adjacency matrix: wins over rest
        X <- adjacency(rankings, weights = weights)

        # sizes of selected sets
        S <- apply(rankings, 1, function(x){
            last <- which(x == max(x))
            ind <- which(x > 0)
            # exclude untied last place
            if (length(last) == 1) ind <- setdiff(ind, last)
            list(x = tabulate(x[ind])[x[ind]], ind = ind)
        })
        ind <- unlist(lapply(S, `[[`, "ind"))
        S <- lapply(S, `[[`, "x")
        ## replicate ranking weight for each choice in ranking
        w <- rep(weights, lengths(S))
        S <- unlist(S)

        # sufficient statistics
        # for alpha i, sum over all sets st object i is in selected set/size of
        # selected set
        A <- numeric(N)
        i <- sort(unique(ind))
        A[i] <- unname(rowsum(w/S, ind)[,1])
        # for delta d, number of sets with cardinality d/cardinality
        B <- as.vector(unname(rowsum(w, S)))
        rm(S, ind)
    } else {
        # adjacency matrix: wins over rest
        # (id extracted from grouped_rankings object)
        X <- matrix(0, N, N)
        for (i in seq_along(id)) X[id[[i]]] <- X[id[[i]]] + weights[i]
        class(X) <- c("adjacency", "matrix")

        # replicate ranking weight for each choice in ranking
        # (S extracted from grouped_rankings object)
        w <- rep(weights, rowSums(S > 0))

        # sufficient statistics
        # for alpha i, sum over all sets st object i is in selected set/size of
        # selected set
        A <- unname(rowsum(w/S[as.logical(S)], R[as.logical(S)])[,1])
        # for delta d, number of sets with cardinality d/cardinality
        B <- tabulate(S[as.logical(S)])
        rm(S)
    }
    D <- length(B)
    B <- B/seq(D)

    # check connectivity if npseudo = 0
    if (npseudo == 0){
        out <- connectivity(X, verbose = FALSE)
        if (out$no > 1)
            stop("Network is not fully connected - cannot estimate all ",
                 "item parameters with npseudo = 0")
    }

    # unique rows with >= 1 element for logical matrix
    # case where p is fixed
    uniquerow <- function(M){
        len <- ncol(M)
        if (len == 1) return(1)
        pattern <- M[,1]
        max_exp <- .Machine$double.digits
        for (k in seq_len(len - 1)){
            pattern <- 2*pattern + M[,k]
            if (k == len - 1 ||
                k %% max_exp == (max_exp - 1)){
                pattern <- match(pattern, unique(pattern))
                max_exp <- .Machine$double.digits - ceiling(log2(max(pattern)))
            }
        }
        pattern
    }

    # max number of objects in set
    nc <- max(rowSums(rankings > 0))

    # create W so cols are potential set sizes and value > 0 indicates ranking
    # aggregate common sets
    # - incorporate ranking weights by weighted count vs simple tabulate
    W <- G <- list() # weight (currently rep count); group of rankings
    nc <- max(rowSums(rankings > 0))
    S <- logical(nc)  # set sizes present in rankings
    minrank <- rep.int(1, nr)
    for (i in seq_len(nc)){
        s <- (nc - i + 1)
        set <- rankings >= minrank
        r <- which(rowSums(set) == s)
        S[s] <- s != 1 && length(r)
        if (!S[s]) next
        g <- uniquerow(set[r, , drop = FALSE])
        W[[s]] <- as.vector(unname(rowsum(weights[r], g)))
        G[[s]] <- r[!duplicated(g)]
        minrank[r] <- minrank[r] + 1
    }
    S <- which(S)

    # if npseudo > 0 add npseudo wins and losses with hypothetical item
    stopifnot(npseudo >= 0)
    if (npseudo > 0){
        # update R with paired comparisons with hypothetical item (item N + 1)
        R <- cbind(R, "NULL" = 0)
        pseudo <- matrix(0, nrow = N, ncol = N + 1)
        pseudo[, 1] <- N + 1
        pseudo[, 2]  <- seq_len(N)
        R <- rbind(R, pseudo)
        # update X with npseudo wins and losses  with hypothetical item
        X <- cbind(X, npseudo)
        X <- rbind(X, c(rep.int(npseudo, N), 0))
        # update weights: 2*npseudo comparisons of each pair
        W[[2]] <- c(W[[2]], rep.int(2*npseudo, N))
        # update indices
        G[[2]] <- c(G[[2]], (nr + 1):(nr + N))
        # update A: npseudo wins for item N + 1; npseudo wins for other items
        A <- c(A + npseudo, N*npseudo)
        # update B: 2*npseudo untied choices
        B[1] <- B[1] + 2*npseudo*N
        N <- N + 1
    }

    # (scaled, un-damped) PageRank based on underlying paired comparisons
    alpha <- drop(abs(eigs(X/colSums(X), 1,
                           opts = list(ncv = min(nrow(X), 10)))$vectors))
    # fix alpha for hypothetical item to 1
    if (npseudo > 0) {
        alpha <- alpha/alpha[N]
    }
    delta <- rep.int(0.1, D)
    delta[1] <- 1

    # quasi-newton methods ---

    key_quantities <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)] # includes delta1
        res <- expectation("all", alpha, c(1, delta), N, D, S, R, G, W)
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
        # will give an error if lbfgs not available
        res <- lbfgs::lbfgs(obj, gr, log(c(alpha, delta[-1])), invisible = 1)
        conv <- res$convergence == 0
        iter <- NULL
        res <- list(alpha = exp(res$par[1:N]),
                    delta = c(1, exp(res$par[-(1:N)])))
    } else {
        res <- list(alpha = alpha, delta = delta)
        res[c("expA", "expB")] <-
            expectation(c("alpha", "delta"), alpha, delta, N, D, S, R, G, W)
        oneUpdate <- function(res){
            # update all alphas
            if (npseudo > 0){
                res$alpha[-1] <- res$alpha[-1]*A[-1]/res$expA[-1]
            } else {
                res$alpha <- res$alpha*A/res$expA
            }
            # update all deltas
            if (D > 1) {
                res$delta[-1] <- B[-1]/
                    expectation("delta", res$alpha, res$delta,
                                N, D, S, R, G, W)$expB
            }
            res[c("expA", "expB")] <-
                expectation(c("alpha", "delta"), res$alpha, res$delta,
                            N, D, S, R, G, W)
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
            eps <- abs(c(A, B[-1]) - c(res$expA, res$delta[-1]*res$expB))
            assign("eps", eps, envir = parent.env(environment()))
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
        eps <- c(A, B[-1])
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
                if (npseudo > 0){
                    res$alpha[-1] <-
                        accelerate(res$alpha, res1$alpha, res2$alpha)[-1]
                } else res$alpha <-
                        accelerate(res$alpha, res1$alpha, res2$alpha)
                res$delta[-1] <-
                    accelerate(res$delta, res1$delta, res2$delta)[-1]
                res[c("expA", "expB")] <-
                    expectation(c("alpha", "delta"), res$alpha, res$delta,
                                N, D, S, R, G, W)
                if (conv <- checkConv(res)) break
            } else{
                updateIter()
            }
        }
    }
    if (!conv)
        warning("Iterations have not converged.")

    res$delta <- structure(res$delta, names = paste0("tie", 1:D))[-1]

    if (npseudo > 0) {
        # drop hypothetical object
        res$alpha <- res$alpha[-N]
        N <- N - 1
        # drop weights and indices for pseudodata
        i <- seq_len(length(W[[2]]) - N)
        W[[2]] <- W[[2]][i]
        G[[2]] <- G[[2]][i]
        if (!length(W[[2]])) S <- setdiff(S, 2)
        # remove contribution to A and B
        A <- A[-(N + 1)] - npseudo
        B[1] <- B[1] - 2*npseudo*N
    }
    res$alpha <- res$alpha/sum(res$alpha)
    rank <- N + D - 2

    # frequencies of sets selected from, for sizes 2 to max observed
    freq <- vapply(W[S], sum, 1)
    # number of possible selections overall
    n <- sum(vapply(S, choose, numeric(D), k = seq(D)) %*% freq)
    df.residual <- n - sum(freq) - rank

    logl <- loglik(unlist(res[c("alpha", "delta")]))

    if (length(res$alpha) < length(items)){
        out <- rep.int(NA_real_, length(items))
        names(out) <- items
        out[colnames(R)] <- res$alpha
        res$alpha <- out
    } else names(res$alpha) <- items

    fit <- list(call = call,
                coefficients = c(res$alpha, res$delta),
                ref = ref,
                loglik = unname(logl),
                df.residual = df.residual,
                rank = rank,
                iter = iter,
                rankings = rankings,
                weights = weights,
                maxTied = D)
    class(fit) <- "PlackettLuce"
    fit
}

# function to compute expectations of the sufficient statistics of the alphas/deltas
# if ranking weight is NULL, do not aggregate across rankings
expectation <- function(par, # par to compute expectations for
                        alpha, # alpha par
                        delta, # delta par
                        N, # number of objects
                        D, # max tie order
                        S, # set sizes in representative rankings
                        R, # items in each ranking, from last to first place
                        G, # group of rankings to include; list for each S
                        W = NULL){ # weight of rankings; list for each S
    keepAlpha <- any(par %in% c("alpha", "all"))
    keepDelta <- D > 1 && any(par %in% c("delta", "all"))
    keepTheta <- any(par %in% c("theta", "all"))
    expA <- expB <- theta <- NULL
    if (keepAlpha) {
        if (!is.null(W)) {
            expA <- numeric(N)
        } else expA <- matrix(0, nrow = nrow(R), ncol = N)
    }
    if (keepDelta) {
        if (!is.null(W)) {
            expB <- numeric(D - 1)
        } else expB <- matrix(0, nrow = nrow(R), ncol = D - 1)
    }
    if (keepTheta) theta <- numeric(sum(lengths(G[S])))
    z <- 1
    for (s in S){
        # D == 1
        ## numerators (for expA, else just to compute denominators)
        r <- G[[s]]
        nr <- length(r)
        x1 <- matrix(alpha[R[r, 1:s]],
                     nrow = nr, ncol = s)
        ## denominators
        z1 <- rowSums(x1)
        # D > 1
        d <- min(D, s)
        if (d > 1){
            if (keepDelta)
                y1 <- matrix(0, nrow = nr, ncol = d - 1)
            # index up to d items: start with 1:n
            i <- seq_len(d)
            # id = index to change next; id2 = first index changed
            if (d == s) {
                id <- s - 1
            } else id <- d
            id2 <- 1
            repeat{
                # work along index vector from 1 to end/first index = s
                v1 <- alpha[R[r, i[1]]] # ability for first ranked item
                last <- i[id] == s
                if (last) {
                    end <- id
                } else end <- min(d, id + 1)
                for (k in 2:end){
                    # product of first k alphas indexed by i
                    v1 <- v1 * alpha[R[r, i[k]]]
                    # ignore if already recorded
                    if (k < id2) next
                    # add to numerators/denominators for sets of order s
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
                if (i[1] == (s - 1)) break
                if (last){
                    id2 <- id - 1
                    v <- i[id2]
                    len <- min(s - 2 - v, d - id2)
                    id <- id2 + len
                    i[id2:id] <- v + seq_len(len + 1)
                } else {
                    id2 <- id
                    i[id] <- i[id] + 1
                }
            }
        }
        # add contribution for sets of size s to expectation
        if (keepAlpha){
            # R[r, 1:s] may only index some alphas
            if (!is.null(W)){
                id <- unique(as.integer(R[r, 1:s]))
                add <- drop(rowsum(as.vector(W[[s]] * x1/z1),
                                   c(R[r, 1:s]), reorder = FALSE))
                expA[id] <- expA[id] + add
            } else {
                id <- cbind(r, c(R[r, 1:s]))
                expA[id] <- expA[id] + c(x1/z1)
            }
        }
        if (keepDelta && s > 1){
            if (!is.null(W)){
                expB[seq_len(d - 1)] <- expB[seq_len(d - 1)] +
                    colSums(W[[s]] * y1/z1)
            } else expB[r, seq_len(d - 1)] <- expB[r, seq_len(d - 1)] + y1/z1
        }
        if (keepTheta){
            if (par == "all"){
                # return logtheta
                if (!is.null(W)){
                    theta[z:(z + nr - 1)] <- W[[s]] * log(z1)
                } else theta[z:(z + nr - 1)] <- log(z1)
            } else {
                if (!is.null(W)){
                    theta[z:(z + nr - 1)] <- W[[s]] * z1
                } else theta[z:(z + nr - 1)] <- z1
            }
            z <- z + nr
        }
    }
    c(list(expA = expA)[keepAlpha],
      list(expB = expB)[keepDelta],
      list(theta = theta)[keepTheta])
}
