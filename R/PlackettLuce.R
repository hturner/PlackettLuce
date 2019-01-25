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
#' @param normal a optional list with elements named `mu` and `Sigma`
#' specifying the mean and covariance matrix of a multivariate normal prior on
#' the _log_ worths.
#' @param gamma a optional list with elements named `shape` and `rate`
#' specifying parameters of a Gamma prior on adherence parameters for each
#' ranker (use `grouped_rankings` to group multiple rankings by ranker). If
#' `NULL`, adherence is fixed to `adherence` for all rankers.
#' @param adherence an optional vector of adherence values for each ranker. If
#' missing, adherence is fixed to 1 for all rankers. If `gamma != NULL` this is
#' taken as the starting values for the adherence.
#' @param weights an optional vector of weights for each ranking.
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
                         normal = NULL,
                         gamma = NULL,
                         adherence = NULL,
                         weights = NULL,
                         start = NULL,
                         method = c("iterative scaling", "BFGS", "L-BFGS"),
                         epsilon = 1e-7, steffensen = 0.1, maxit = 500,
                         trace = FALSE, verbose = TRUE, ...){
    call <- match.call()

    # check rankings
    grouped_rankings <- inherits(rankings, "grouped_rankings")
    if (grouped_rankings){
        ranker <- attr(rankings, "index")
        # weights are per group id - expand to be per ranking
        if (!is.null(weights)) {
            stopifnot(length(weights) == max(ranker))
            stopifnot(all(weights > 0))
            weights <- weights[ranker]
        }
        # same for adherence
        if (!is.null(adherence)) {
            stopifnot(length(adherence) == max(ranker))
            stopifnot(all(adherence > 0))
            adherence <- adherence[ranker]
        }
        R <- attr(rankings, "R")
        S <- attr(rankings, "S")
        id <- attr(rankings, "id")
        rankings <- attr(rankings, "rankings")
    } else if (!inherits(rankings, "rankings")){
        rankings <- suppressWarnings(as.rankings(rankings, verbose = verbose))
        if (!is.null(adherence)) ranker <- seq_len(nrow(rankings))
    }

    # attributes
    items <- colnames(rankings)
    N <- ncol(rankings) # total number of objects
    nr <- nrow(rankings) # number of rankings

    # weights
    if (is.null(weights)){
        weights <- rep.int(1, nr)
    } else stopifnot(length(weights) == nr)

    # adherence
    if (!is.null(adherence)){
        stopifnot(length(adherence) == nr)
        a <- adherence
    } else if (!is.null(gamma)) {
        adherence <- a <- rep.int(1, length(weights))
    } else a <- NULL

    if (!grouped_rankings){
        # items ranked from last to 1st place
        R <- t(apply(rankings, 1, order, decreasing = TRUE))

        # adjacency matrix: wins over rest
        # N.B. need even if `start` specified; used to check connectivity
        X <- adjacency(rankings, weights = weights)

        # sizes of selected sets
        set <- apply(rankings, 1, function(x){
            last <- which(x == max(x))
            ind <- which(x > 0)
            # exclude untied last place
            if (length(last) == 1) ind <- setdiff(ind, last)
            list(size = tabulate(x[ind])[x[ind]], item_id = ind)
        })
        item_id <- unlist(lapply(set, `[[`, "item_id"))
        S <- lapply(set, `[[`, "size")
        rm(set)
    } else {
        # adjacency matrix: wins over rest
        # (id extracted from grouped_rankings object)
        X <- matrix(0, N, N)
        for (i in seq_along(id)) X[id[[i]]] <- X[id[[i]]] + weights[i]
        class(X) <- c("adjacency", "matrix")

        # (S extracted from grouped_rankings object)
        item_id <- R[as.logical(S)]
        S <- split(S[as.logical(S)], row(R)[as.logical(S)])
    }
    # sufficient statistics
    # for delta d, (number of sets with cardinality d)/cardinality
    w <- rep.int(weights, lengths(S))
    B <- unname(rowsum(w, unlist(S))[,1])
    D <- length(B)
    B <- B/seq(D)
    # from now only only need weight/size per set, so replace S with this
    S <- Map("/", weights, S)
    # for alpha
    A <- numeric(N)
    item <- sort(unique(item_id))
    if (is.null(adherence)){
        # sum over all sets st object i is in selected set weight/size
        A[item] <- unname(rowsum(unlist(S), item_id)[,1])
    } else {
        # now (adherence * weight)/size
        A[item] <- unname(rowsum(unlist(Map("*", a, S)), item_id)[,1])
        # for adherence, sum(log-worth * weight/size) for all chosen items
        # calculate later, if estimating adherence
        if (!is.null(gamma)) ranker_id <- rep.int(ranker, lengths(S))
    }


    if (!is.null(normal)){
        # set npseudo to 0
        npseudo <- 0
        # check normal prior specification
        stopifnot(names(normal) == c("mu", "Sigma"))
        if (length(normal$mu) != N)
            stop("`length(normal$mu)` is not equal to the number of items")
        if (!identical(dim(normal$Sigma), c(N, N)))
            stop("`normal$Sigma` is not a square matrix with number of rows ",
                 "equal to the number of items")
        # compute inverse of variance covariance matrix (used in score function)
        Rinv <- solve(chol(normal$Sigma))
        Kinv <- Rinv %*% t(Rinv)
    } else Rinv <- Kinv <- NULL

    # check connectivity if npseudo = 0 and normal prior not specified
    if (npseudo == 0 & is.null(normal)){
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
    P <- logical(nc)  # set sizes present in rankings
    minrank <- rep.int(1, nr)
    for (i in seq_len(nc)){
        p <- (nc - i + 1)
        set <- rankings >= minrank
        r <- which(rowSums(set) == p)
        P[p] <- p != 1 && length(r)
        if (!P[p]) next
        if (!is.null(gamma) & !grouped_rankings){
            W[[p]] <- weights[r]
            G[[p]] <- r
        } else {
            g <- uniquerow(set[r, , drop = FALSE])
            if (!is.null(adherence)){
                x <- ranker[r]/log10(max(ranker[r]) + 1) + g # combine groupings
                g <- match(x, x)
            }
            W[[p]] <- as.vector(unname(rowsum(weights[r], g)))
            G[[p]] <- r[!duplicated(g)]
        }
        minrank[r] <- minrank[r] + 1
    }
    P <- which(P)

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
        if (!is.null(gamma)) a <- c(a, rep.int(1, N))
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
        if (!is.null(normal)) {
            alpha <- exp(normal$mu)
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
    if ((!is.null(normal) | !is.null(gamma)) && method == "iterative scaling"){
        if (method %in% names(call)){
            stop("when a prior is specified, `method` cannot be ",
                 "`\"iterative scaling\"`")
        } else method <- "BFGS"
    }

    if (method == "BFGS"){
        opt <- function(par, obj, gr, ...){
            dts <- list(...)
            do.call("optim",
                    c(list(par, obj, gr, method = "BFGS",
                           control = c(eval(dts$control), list(maxit = maxit))),
                           dts[setdiff(names(dts), "control")]))
        }
    }

    if (method == "L-BFGS"){
        opt <- function(par, obj, gr, ...){
            lbfgs::lbfgs(obj, gr, par, invisible = 1,
                         max_iterations = maxit, ...)
        }
    }

    # Optimization of log-worths and log-tie parameters via
    obj_common <- function(par) {
        alpha <- exp(par[1:N])
        delta <- exp(par[-c(1:N)])
        # assign to parent environment so can use further quantities in score
        assign("fit", expectation("all", alpha, c(1, delta),
                                  a, N, D, P, R, G, W),
               envir = parent.env(environment()))
        -loglik_common(c(alpha, delta), N, normal$mu, Kinv, A, B, fit)
    }
    gr_common <- function(par) {
        alpha <- exp(par[1:N])
        delta <- exp(par[-c(1:N)])
        -score_common(c(alpha, delta), N, normal$mu, Rinv, A, B, fit) *
            c(alpha, delta)
    }

    # Optimization of log-adherence via
    obj_adherence <- function(par){
        adherence <- exp(par)
        # assign to parent environment so can use results in score
        assign("fit", normalization(alpha, c(1, delta),
                                    adherence[ranker], D, P, R, G, W),
               envir = parent.env(environment()))
        -loglik_adherence(adherence, shape, rate, Z)
    }
    gr_adherence <- function(par) {
        adherence <- exp(par)
        -score_adherence(adherence, shape, rate, Z) * adherence
    }

    if (method != "iterative scaling"){
        for (i in 1){ # will need to set convergence criterion
            # fit model with fixed adherence (a)
            res <- opt(log(c(alpha, delta[-1])), obj_common, gr_common, ...)

            if (!is.null(gamma)){
                # update alpha, delta & sufficient statistics for adherence
                alpha <- exp(res$par[1:N])
                delta <- exp(res$par[-(1:N)])
                Z <- unname(rowsum(unlist(S)*log(alpha[item_id]),
                                   ranker_id)[,1])

                # fit model with fixed worth/tie parameters
                # ignore rankings involving ghost item (adherence fixed to 1)
                res2 <- opt(log(adherence), obj_adherence, gr_adherence, ...)

                # update adherence & sufficient statistics for alpha (real items)
                a[1:nr] <- res2$adherence[ranker]
                A[item] <- unname(rowsum(unlist(Map("*", a[1:nr], S)),
                                         item_id)[,1])
            }
        }
        conv <- res$convergence
        iter <- res$counts # NULL for L-BFGS
        res <- list(alpha = exp(res$par[1:N]),
                    delta = c(1, exp(res$par[-(1:N)])),
                    logl = -res$value)
        if (!is.null(gamma)) res$obj <- -res2$value
    } else {
        res <- list(alpha = alpha, delta = delta)
        res[c("expA", "expB", "theta")] <-
            expectation("all", alpha, delta, a, N, D, P, R, G, W)
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
                                a, N, D, P, R, G, W)$expB
            }
            res[c("expA", "expB", "theta")] <-
                expectation("all", res$alpha, res$delta,
                            a, N, D, P, R, G, W)
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
                                    a, N, D, P, R, G, W)
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
        if (!length(W[[2]])) P <- setdiff(P, 2)
        # remove contribution to A and B
        A <- A[-(N + 1)] - npseudo
        B[1] <- B[1] - 2*npseudo*N
        # remove adherence for ghost rater
        if (!is.null(adherence)) adherence <- adherence[seq(N)]
    }
    names(res$alpha) <- items
    rank <- N + D - 2

    cf <- c(res$alpha, res$delta)

    # recompute log-likelihood excluding pseudo-observations/normal prior
    logp <- NULL
    if (npseudo > 0 | !is.null(normal)) {
        normal <- NULL
        logp <- res$logl
        logl <- -obj_common(log(cf))
    } else logl <- res$logl
    # null log-likelihood
    null.loglik <- -obj_common(rep.int(0, length(cf)))

    # frequencies of sets selected from, for sizes 2 to max observed
    freq <- vapply(W[P], sum, 1)
    # number of possible selections overall
    n <- sum(vapply(P, choose, numeric(D), k = seq(D)) %*% freq)
    df.residual <- n - sum(freq) - rank

    fit <- list(call = call,
                coefficients = cf,
                loglik = logl,
                null.loglik = null.loglik,
                df.residual = df.residual,
                df.null = n - sum(freq), #naming consistent with glm
                rank = rank,
                logposterior = logp,
                obj = res$obj,
                iter = iter,
                rankings = rankings,
                weights = weights,
                adherence = adherence,
                maxTied = D,
                conv = conv)
    class(fit) <- "PlackettLuce"
    fit
}

