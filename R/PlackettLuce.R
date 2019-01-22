#' Fit a Plackett-Luce Model
#'
#' Fit a Plackett-Luce model to a set of rankings. The rankings may be partial
#' (not all objects ranked) and include ties of any order.
#'
#' @section Model definition:
#'
#' A single ranking is given by
#' \deqn{R = \{C_1, C_2, \ldots, C_J\}}{R = {C_1, C_2, \ldots, C_J}}
#' where the items in set \eqn{C_1} are ranked higher than (better than) the
#' items in \eqn{C_2}, and so on. If there are multiple objects in set \eqn{C_j}
#' these items are tied in the ranking.
#'
#' For a set if items \eqn{S}, let
#' \deqn{f(S) = \delta_{|S|}
#'       \left(\prod_{i \in S} \alpha_i \right)^\frac{1}{|S|}}{
#'       f(S) = d_{|S|} * (prod_{i in S} a_i)^(1/|S|)}
#' where \eqn{|S|} is the cardinality (size) of the set, \eqn{\delta_n}{d_n} is a
#' parameter representing the prevalence of ties of order \eqn{n}, and
#' \eqn{\alpha_i}{a_i} is a parameter representing the worth of item \eqn{i}.
#' Then under an extension of the Plackett-Luce model allowing ties up to order
#' \eqn{D}, the probability of the ranking \eqn{R} is given by
#' \deqn{\prod_{j = 1}^J \frac{f(C_j)}{
#'       \sum_{k = 1}^{\min(D_j, D)} \sum_{S \in {A_j \choose k}} f(S)}}{
#'       prod_{j = 1}^J f(C_j)/
#'       (sum_{k = 1}^{min(D_j, D)} sum_{S in choose(A_j, k)} f(S))}
#' where \eqn{D_j} is the cardinality of \eqn{C_j}, \eqn{A_j} is the set of
#' alternatives from which \eqn{C_j} is chosen, and
#' \eqn{A_j \choose k}{choose(A_j, k)} is all the possible choices of \eqn{k}
#' items from \eqn{A_j}. The value of \eqn{D} can be set to the maximum number
#' of tied items observed in the data, so that \eqn{\delta_n = 0}{d_n = 0} for
#' \eqn{n > D}.
#'
#' When the worth parameters are constrained to sum to one, they represent the
#' probability that the corresponding item comes first in a ranking of all
#' items, given that first place is not tied.
#'
#' The 2-way tie prevalence parameter \eqn{\delta_2}{d_2}$ is interpretable
#' via the probability that two given items \emph{of equal worth} tie for
#' first place, given that the first place is not a 3-way or higher tie.
#' Specifically, that probability is
#' \eqn{\delta_2/(2 + \delta_2)}{d_2/(2 + d_2}.
#'
#' The 3-way and higher tie-prevalence parameters are interpretable similarly,
#' in terms of tie probabilities among equal-worth items.
#'
#' @section Pseudo-rankings:
#'
#' In order for the maximum likelihood estimate of an object's worth to be
#' defined, the network of rankings must be strongly connected. This means that
#' in every possible partition of the objects into two nonempty subsets, some
#' object in the second set is ranked higher than some object in the first set
#' at least once.
#'
#' If the network of rankings is not strongly connected then pseudo-rankings
#' may be used to connect the network. This approach posits a hypothetical
#' object with log-worth 0 and adds \code{npseudo} wins and \code{npseudo}
#' losses to the set of rankings.
#'
#' The parameter \code{npseudo} is the prior strength.  With \code{npseudo = 0}
#' the MLE is the posterior mode.  As \code{npseudo} approaches
#' infinity the log-worth estimates all shrink towards 0. The default,
#' \code{npseudo = 0.5}, is sufficient to connect the network and has a weak
#' shrinkage effect. Thus even for networks that are already connected, adding
#' pseudo-rankings reduces both the bias and variance of the estimates.
#'
#' @section Incorporating prior information:
#'
#' Prior information can be incorporated by using `prior` to specify a
#' multivariate normal prior on the log-worths. The log-worths are then
#' estimated by maximum apriori estimation. Model summaries (deviance, AIC,
#' standard errors) are based on the log-likelihood evaluated at the MAP
#' estimates, resulting in a finite sample bias that should disappear as
#' the number of rankings increases. Inference based on these model summaries
#' is valid as long as the prior is considered fixed and not tuned as part of
#' the model.
#'
#' Incorporating a prior is an alternative method of penalization, therefore
#' `npseudo` is set to zero when a prior is specified.
#'
#' @section Controlling the fit:
#'
#' Using \code{nspseudo = 0} will use standard maximum likelihood, if the
#' network is connected (and throw an error otherwise).
#'
#' The fitting algorithm is set by the \code{method} argument. The default
#' method \code{"iterative scaling"} is a slow but reliable approach. In
#' addition, this has the most control on the accuracy of the final fit, since
#' convergence is determined by direct comparison of the observed and expected
#' values of the sufficient statistics for the worth parameters, rather than a
#' tolerance on change in the log-likelihood.
#'
#' The \code{"iterative scaling"} algorithm is slow because it is a first order
#' method (does not use derivatives of the likelihood). From a set of starting
#' values that are 'close enough' to the final solution, the algorithm can be
#' accelerated using
#' \href{https://en.wikipedia.org/wiki/Steffensen\%27s_method}{Steffensen's method}.
#' \code{PlackettLuce} attempts to apply Steffensen's acceleration when all
#' differences between the observed and expected values of the sufficient
#' statistics are less than \code{steffensen}. This is an ad-hoc rule defining
#' 'close enough' and in some cases the acceleration may produce negative
#' worth parameters or decrease the log-likelihood. \code{PlackettLuce} will
#' only apply the update when it makes an improvement.
#'
#' The \code{"BFGS"} and \code{"L-BFGS"} algorithms are second order methods,
#' therefore can be quicker than the default method. Control parameters can be
#' passed on to \code{\link[stats]{optim}} or \code{\link[lbfgs]{lbfgs}}.
#'
#' @seealso
#'
#' Handling rankings: \code{\link{rankings}}, \code{choices}, \code{adjacency},
#' \code{connectivity}.
#'
#' Inspect fitted Plackett-Luce models: \code{\link{coef}}, \code{deviance},
#' \code{\link{fitted}}, \code{\link{itempar}}, \code{logLik}, \code{print},
#' \code{\link{qvcalc}}, \code{\link{summary}}, \code{\link{vcov}}.
#'
#' Fit Plackett-Luce tree: \code{\link{grouped_rankings}}, \code{pltree}.
#'
#' Example data sets: \code{\link{beans}}, \code{\link{nascar}},
#' \code{\link{pudding}}, \code{\link{read.soc}}.
#'
#' Vignette: \code{vignette("Overview", package = "PlackettLuce")}.
#'
#' @param rankings a \code{"\link{rankings}"} object, or an object that can be
#' coerced by \code{as.rankings}.
#' @param npseudo when using pseudodata: the number of wins and losses to add
#' between each object and a hypothetical reference object.
#' @param prior a optional list with elements named `mu` and `Sigma` specifying
#' the mean and covariance matrix of a multivariate normal prior on the
#' _log_ worths.
#' @param weights an optional vector of weights for each ranking.
#' @param adherence an optional vector of adherence values for each ranking.
#' @param start starting values for the worth parameters and the tie parameters
#' on the raw scale (worth parameters need not be scaled to sum to 1). If
#' `prior` is specified, `exp(prior$mu)` is used starting values for the worth
#' parameters. Coefficients from a previous fit can be passed as the result of
#' a call to  \code{coef.PlackettLuce}, or the \code{coefficients} element of a
#' \code{"PlackettLuce"} object.
#' @param method  the method to be used for fitting: \code{"iterative scaling"} (default: iterative scaling to sequentially update the parameter values), \code{"BFGS"} (the BFGS optimisation algorithm through the \code{\link{optim}} interface), \code{"L-BFGS"} (the limited-memory BFGS optimisation algorithm as implemented in the \code{\link[lbfgs]{lbfgs}} package).
#' @param epsilon the maximum absolute difference between the observed and
#' expected sufficient statistics for the ability parameters at convergence.
#' @param steffensen a threshold defined as for \code{epsilon} after which to
#' apply Steffensen acceleration to the iterative scaling updates.
#' @param maxit the maximum number of iterations.
#' @param trace logical, if \code{TRUE} show trace of iterations.
#' @param verbose logical, if \code{TRUE} show messages from validity checks on
#' the rankings.
#' @param ... additional arguments passed to \code{optim} or \code{lbfgs}.
#' In particular the convergence tolerance may be adjusted using e.g.
#' \code{control = list(reltol = 1e-10)}.
#'
#' @return An object of class \code{"PlackettLuce"}, which is a list containing the
#' following elements:
#' \item{call}{ The matched call. }
#' \item{coefficients}{ The model coefficients. }
#' \item{loglik}{ The maximized log-likelihood. }
#' \item{null.loglik}{ The maximized log-likelihood for the null model (all
#' alternatives including ties have equal probability). }
#' \item{df.residual}{ The residual degrees of freedom. }
#' \item{df.null}{ The residual degrees of freedom for the null model. }
#' \item{rank}{ The rank of the model. }
#' \item{logposterior}{ If a prior was specified, the maximised log posterior.}
#' \item{iter}{ The number of iterations run. }
#' \item{rankings}{ The rankings passed to \code{rankings}, converted to a
#' \code{"rankings"} object if necessary. }
#' \item{weights}{ The weights applied to each ranking in the fitting. }
#' \item{maxTied}{ The maximum number of objects observed in a tie. }
#' \item{conv}{ The convergence code: 0 for successful convergence; 1 if reached
#' \code{maxit} iterations without convergence; 2 if Steffensen acceleration
#' cause log-likelihood to increase; negative number if L-BFGS algorithm failed
#' for other reason.}
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
PlackettLuce <- function(rankings,
                         npseudo = 0.5,
                         prior = NULL,
                         weights = NULL,
                         adherence = NULL,
                         start = NULL,
                         method = c("iterative scaling", "BFGS", "L-BFGS"),
                         epsilon = 1e-7, steffensen = 0.1, maxit = 500,
                         trace = FALSE, verbose = TRUE, ...){
    call <- match.call()

    # check rankings
    grouped_rankings <- inherits(rankings, "grouped_rankings")
    if (grouped_rankings){
        # weights are per group id - expand to be per ranking
        if (!is.null(weights)) {
            stopifnot(length(weights) == max(attr(rankings, "index")))
            stopifnot(all(weights > 0))
            weights <- weights[attr(rankings, "index")]
        }
        # same for adherence
        if (!is.null(adherence)) {
            stopifnot(length(adherence) == max(attr(rankings, "index")))
            stopifnot(all(adherence > 0))
            adherence <- adherence[attr(rankings, "index")]
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

    # adherence
    if (!is.null(adherence)){
       stopifnot(length(adherence) == nrow(adherence))
    }

    if (!grouped_rankings){
        # items ranked from last to 1st place
        R <- t(apply(rankings, 1, order, decreasing = TRUE))

        # adjacency matrix: wins over rest
        # N.B. need even if `start` specified; used to check connectivity
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
        ## replicate ranking weight and adherences for each choice in ranking
        w <- rep(weights, lengths(S))
        if (!is.null(adherence)) a <- rep(adherence, lengths(S))
        S <- unlist(S)

        # sufficient statistics
        # for alpha i, sum over all sets st object i is in selected set/size of
        # selected set (weighted by adherence)
        A <- numeric(N)
        i <- sort(unique(ind))
        if (!is.null(adherence)){
            A[i] <- unname(rowsum((a*w)/S, ind)[,1])
        } else A[i] <- unname(rowsum(w/S, ind)[,1])
        # for delta d, number of sets with cardinality d/cardinality
        B <- as.vector(unname(rowsum(w, S)))
        rm(S, ind)
    } else {
        # adjacency matrix: wins over rest
        # (id extracted from grouped_rankings object)
        X <- matrix(0, N, N)
        for (i in seq_along(id)) X[id[[i]]] <- X[id[[i]]] + weights[i]
        class(X) <- c("adjacency", "matrix")

        # replicate ranking weight and adherence for each choice in ranking
        # (S extracted from grouped_rankings object)
        w <- rep(weights, rowSums(S > 0))
        if (!is.null(adherence)) a <- rep(adherence, rowSums(S > 0))

        # sufficient statistics
        # for alpha i, sum over all sets st object i is in selected set/size of
        # selected set (weighted by adherence)
        A <- numeric(N)
        i <- sort(unique(R[as.logical(S)]))
        if (!is.null(adherence)){
            A[i] <- unname(rowsum((a*w)/S[as.logical(S)], R[as.logical(S)])[,1])
        } else A[i] <- unname(rowsum(w/S[as.logical(S)], R[as.logical(S)])[,1])
        # for delta d, number of sets with cardinality d/cardinality
        B <- tabulate(S[as.logical(S)])
        rm(S)
    }
    D <- length(B)
    B <- B/seq(D)

    # set nspeudo to 0 if prior is specified
    if (!is.null(prior)){
        npseudo <- 0
        stopifnot(names(prior) == c("mu", "Sigma"))
        if (length(prior$mu) != N)
            stop("`length(prior$mu)` is not equal to the number of items")
        if (!identical(dim(prior$Sigma), c(N, N)))
            stop("`prior$Sigma` is not a square matrix with number of rows ",
                 "equal to the number of items")

    }

    # check connectivity if npseudo = 0 and prior not specified
    if (npseudo == 0 & is.null(prior)){
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
        if (!is.null(adherence)){
            x <- adherence[r]/log10(max(adherence[r]) + 1) + g # combine groupings
            g <- match(x, x)
        }
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
        # update adherence: set to 1 for ghost rater
        if (!is.null(adherence)) adherence <- c(adherence, rep.int(1, N))
        # update weights: 2*npseudo comparisons of each pair
        W[[2]] <- c(W[[2]], rep.int(2*npseudo, N))
        # update indices
        G[[2]] <- c(G[[2]], (nr + 1):(nr + N))
        # update A: npseudo wins for item N + 1 against each other item;
        # npseudo wins for other items
        A <- c(A + npseudo, N*npseudo)
        # update B: 2*npseudo untied choices per item
        B[1] <- B[1] + 2*npseudo*N
    }

    if (is.null(start)){
        if (!is.null(prior)) {
            alpha <- prior$mu
        } else {
            # (scaled, un-damped) PageRank based on underlying paired comparisons
            alpha <- drop(abs(eigs(X/colSums(X), 1,
                                   opts = list(ncv = min(nrow(X), 10)))$vectors))
        }
        delta <- c(1, rep.int(0.1, D - 1))
    } else {
        # if not "coef.PlackettLuce" object, assume on raw scale
        # (with ability of hypothetical item 1), i.e. as returned coefficients)
        alpha <- start[seq_len(N)]
        delta <- start[-seq_len(N)]
        if (inherits(start, "coef.PlackettLuce")){
            # reverse identifiability constraints and put on raw scale
            if (attr(start, "log")){
                alpha <- exp(alpha + attr(start, "const"))
                delta <- exp(delta)
            } else {
                alpha <- alpha*attr(start, "const")
            }
        }
        delta <- c(1, delta)
        if (npseudo > 0) {
            # set alpha for hypothetical item to 1 as in original fit
            if (inherits(start, "coef.PlackettLuce")){
                alpha <- c(alpha, 1)
            } else {
                # set to geometric mean as neutral position
                alpha <- c(alpha, exp(mean(log(alpha))))
            }
        }
    }
    if (npseudo) {
        N <- N + 1
        alpha <- alpha/alpha[N]
    }

    method <- match.arg(method, c("iterative scaling", "BFGS", "L-BFGS"))
    if (!is.null(prior) && method == "iterative scaling"){
        if (method %in% names(call)){
            stop("when `prior` is specified, `method` cannot be ",
                 "`\"iterative scaling\"`")
        } else method <- "BFGS"
    }

    # quasi-newton methods ---

    key_quantities <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)] # includes delta1
        res <- expectation("all", alpha, c(1, delta), N, D, S, R, G,
                           adherence, W)
        # sum of (log(normalising_constants_per_set) * rep)
        c_contr <- sum(res$theta)
        list(alpha = alpha, delta = delta, c_contr = c_contr,
             expA = res$expA, expB = res$expB)
    }

    # Design loglik as brglm2::brglmFit
    # log-likelihood and score functions
    # Within optim or nlminb use obj and gr wrappers below
    #
    if (!is.null(prior)){
        Rinv <- solve(chol(prior$Sigma))
        Kinv <- Rinv %*% t(Rinv)
    }
    # assign key quantities to function environment to re-use
    loglik <- function(par) {
        assign("fit", key_quantities(par), envir = parent.env(environment()))
        res <- sum(B[-1]*log(fit$delta)) + sum(A*log(fit$alpha))- fit$c_contr
        if (is.null(prior)) return(res)
        # -0.5 * (s - mu)^T Sigma^{-1} (s - mu) + standard logL
        res - 0.5*tcrossprod((log(fit$alpha) - prior$mu) %*% Rinv)[1]
    }

    # log-likelihood derivatives
    score <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)]
        res <- c(A/alpha - fit$expA/alpha, B[-1]/delta - fit$expB)
        if (is.null(prior)) return(res)
        # deriv first part wrt log alpha (s) : [-1/nobs Sigma^{-1} (s - mu)]
        res[1:N] <- res[1:N] - Kinv %*% (log(alpha) - prior$mu)/alpha
        res
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

    if (method == "BFGS"){
        contr <- call$control
        res <- optim(log(c(alpha, delta[-1])), obj, gr, method = "BFGS",
                     control = c(eval(contr), list(maxit = maxit)))
        conv <- res$convergence
        iter <- res$counts
        res <- list(alpha = exp(res$par[1:N]),
                    delta = c(1, exp(res$par[-(1:N)])),
                    logl = -res$value)

    } else if (method == "L-BFGS"){
        # will give an error if lbfgs not available
        res <- lbfgs::lbfgs(obj, gr, log(c(alpha, delta[-1])), invisible = 1,
                            max_iterations = maxit, ...)
        conv <- res$convergence
        iter <- NULL
        res <- list(alpha = exp(res$par[1:N]),
                    delta = c(1, exp(res$par[-(1:N)])),
                    logl = -res$value)
    } else {
        res <- list(alpha = alpha, delta = delta)
        res[c("expA", "expB", "theta")] <-
            expectation("all", alpha, delta, N, D, S, R, G, adherence, W)
        res$logl <- sum(B[-1]*log(res$delta)[-1]) + sum(A*log(res$alpha)) -
            sum(res$theta)
        oneUpdate <- function(res){
            # update all alphas
            res$alpha <- res$alpha*A/res$expA
            if (npseudo) res$alpha <- res$alpha/res$alpha[N]
            # update all deltas
            if (D > 1) {
                res$delta[-1] <- B[-1]/
                    expectation("delta", res$alpha, res$delta,
                                N, D, S, R, G, adherence, W)$expB
            }
            res[c("expA", "expB", "theta")] <-
                expectation("all", res$alpha, res$delta,
                            N, D, S, R, G, adherence, W)
            res$logl <- sum(B[-1]*log(res$delta)[-1]) + sum(A*log(res$alpha)) -
                sum(res$theta)
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
            ifelse(all(eps < epsilon), 0, 1)
        }
        iter <- 0
        if ((conv <- checkConv(res)) == 0) maxit <- 0
        updateIter <- function(res){
            if (trace) message("iter ", iter, ", loglik: ", res$logl)
            assign("iter", iter + 1,
                   envir = parent.env(environment()))
        }
        eps <- c(A, B[-1])
        doSteffensen <- FALSE
        while(iter < maxit){
            updateIter(res)
            res <- oneUpdate(res)
            if ((conv <- checkConv(res)) == 0) break
            if (all(eps < steffensen & !doSteffensen)) doSteffensen <- TRUE
            # steffensen
            if (doSteffensen){
                res1 <- oneUpdate(res)
                if ((conv <- checkConv(res1)) == 0) {
                    res <- res1
                    break
                }
                res2 <- oneUpdate(res1)
                if ((conv <- checkConv(res2)) == 0) {
                    res <- res2
                    break
                }
                # if negative worth or log-likelihood decreased,
                # don't apply Steffensen
                res$alpha <-
                    accelerate(res$alpha, res1$alpha, res2$alpha)
                if (all(res$alpha > 0)) {
                    res$delta[-1] <-
                        accelerate(res$delta, res1$delta, res2$delta)[-1]
                    res[c("expA", "expB", "theta")] <-
                        expectation("all", res$alpha, res$delta,
                                    N, D, S, R, G, adherence, W)
                    res$logl <-
                        sum(B[-1]*log(res$delta)[-1]) + sum(A*log(res$alpha)) -
                        sum(res$theta)
                    if (res$logl < res2$logl) {
                        res <- res2
                    } else if ((conv <-  checkConv(res)) == 0) break
                } else res <- res2
            }
        }
    }
    if (trace) message("iter ", iter, ", loglik: ", res$logl)
    if (conv == 1) warning("Iterations have not converged.")

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
        # remove adherence for ghost rater
        if (!is.null(adherence)) adherence <- adherence[seq(N)]
    }
    names(res$alpha) <- items
    rank <- N + D - 2

    cf <- c(res$alpha, res$delta)

    # recompute log-likelihood excluding pseudo-observations/prior component
    logp <- NULL
    if (npseudo > 0 | !is.null(prior)) {
        prior <- NULL
        logp <- res$logl
        logl <- loglik(cf)
    } else logl <- res$logl
    # null log-likelihood
    null.loglik <- loglik(rep.int(1, length(cf)))

    # frequencies of sets selected from, for sizes 2 to max observed
    freq <- vapply(W[S], sum, 1)
    # number of possible selections overall
    n <- sum(vapply(S, choose, numeric(D), k = seq(D)) %*% freq)
    df.residual <- n - sum(freq) - rank

    fit <- list(call = call,
                coefficients = cf,
                loglik = logl,
                null.loglik = null.loglik,
                df.residual = df.residual,
                df.null = n - sum(freq), #naming consistent with glm
                rank = rank,
                logposterior = logp,
                iter = iter,
                rankings = rankings,
                weights = weights,
                adherence = adherence,
                maxTied = D,
                conv = conv)
    class(fit) <- "PlackettLuce"
    fit
}

# function to compute expectations of the sufficient statistics of the alphas/deltas
# (for fixed adherence) if ranking weight is NULL, do not aggregate across rankings
expectation <- function(par, # par to compute expectations for
                        alpha, # alpha par
                        delta, # delta par
                        N, # number of objects
                        D, # max tie order
                        S, # set sizes in representative rankings
                        R, # items in each ranking, from last to first place
                        G, # group of rankings to include; list for each S
                        a = NULL, # adherence for each ranking
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
        if (!is.null(a)) x1 <- x1^a[r]
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
                    if (!is.null(a)) {
                        v2 <- v1^(a[r]/k)
                    } else v2 <- v1^(1/k)
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
    list(expA = if (keepAlpha) expA, expB = if (keepDelta) expB,
         theta = if (keepTheta) theta)
}
