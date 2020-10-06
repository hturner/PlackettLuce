#' Fit a Plackett-Luce Model
#'
#' Fit a Plackett-Luce model to a set of rankings. The rankings may be partial
#' (each ranking completely ranks a subset of the items) and include ties of
#' arbitrary order.
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
#'       f(S) = delta_{|S|} * (prod_{i in S} alpha_i)^(1/|S|)}
#' where \eqn{|S|} is the cardinality (size) of the set, \eqn{\delta_n}{delta_n}
#' is a parameter related to the prevalence of ties of order \eqn{n}
#' (with \eqn{\delta_1 \equiv 1}), and \eqn{\alpha_i}{alpha_i} is a
#' parameter representing the worth of item \eqn{i}.
#' Then under an extension of the Plackett-Luce model allowing ties up to order
#' \eqn{D}, the probability of the ranking \eqn{R} is given by
#' \deqn{\prod_{j = 1}^J \frac{f(C_j)}{
#'       \sum_{k = 1}^{\min(D_j, D)} \sum_{S \in {A_j \choose k}} f(S)}}{
#'       prod_{j = 1}^J f(C_j)/
#'       (sum_{k = 1}^{min(D_j, D)} sum_{S in choose(A_j, k)} f(S))}
#' where \eqn{D_j} is the cardinality of \eqn{A_j}, the set of
#' alternatives from which \eqn{C_j} is chosen, and
#' \eqn{A_j \choose k}{choose(A_j, k)} is all the possible choices of \eqn{k}
#' items from \eqn{A_j}. The value of \eqn{D} can be set to the maximum number
#' of tied items observed in the data, so that \eqn{\delta_n = 0}{delta_n = 0}
#' for \eqn{n > D}.
#'
#' When the worth parameters are constrained to sum to one, they represent the
#' probability that the corresponding item comes first in a ranking of all
#' items, given that first place is not tied.
#'
#' The 2-way tie prevalence parameter \eqn{\delta_2}{delta_2} is related to
#' the probability that two items \emph{of equal worth} tie for
#' first place, given that the first place is not a 3-way or higher tie.
#' Specifically, that probability is
#' \eqn{\delta_2/(2 + \delta_2)}{delta_2/(2 + delta_2}.
#'
#' The 3-way and higher tie-prevalence parameters are similarly interpretable,
#' in terms of tie probabilities among equal-worth items.
#'
#' When intermediate tie orders are not observed (e.g. ties of order 2
#' and order 4 are observed, but no ties of order 3), the maximum
#' likelihood estimate of the corresponding tie prevalence parameters
#' is zero, so these parameters are excluded from the model.
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
#' shrinkage effect. Even for networks that are already connected, adding
#' pseudo-rankings typically reduces both the bias and variance of the
#' estimators of the worth parameters.
#'
#' @section Incorporating prior information on log-worths:
#'
#' Prior information can be incorporated by using \code{normal} to specify a
#' multivariate normal prior on the \emph{log}-worths. The log-worths are then
#' estimated by maximum a posteriori (MAP) estimation. Model summaries
#' (deviance, AIC, standard errors) are based on the log-likelihood evaluated
#' at the MAP estimates, resulting in a finite sample bias that should
#' disappear as the number of rankings increases. Inference based on these
#' model summaries is valid as long as the prior is considered fixed and not
#' tuned as part of the model.
#'
#' Incorporating a prior is an alternative method of penalization, therefore
#' \code{npseudo} is set to zero when a prior is specified.
#'
#' @section Incorporating ranker adherence parameters:
#'
#' When rankings come from different rankers, the model can be extended to
#' allow for varying reliability of the rankers, as proposed by Raman and
#' Joachims (2014). In particular, replacing \eqn{f(S)} by
#' \deqn{h(S) = \delta_{|S|}
#'       \left(\prod_{i \in S} \alpha_i \right)^\frac{\eta_g}{|S|}}{
#'       h(S) = delta_{|S|} * (prod_{i in S} alpha_i)^(eta_g/|S|)}
#' where \eqn{\eta_g > 0}{eta_g > 0} is the adherence parameter for ranker
#' \eqn{g}. In the standard model, all rankers are assumed to have equal
#' reliability, so \eqn{\eta_g = 1}{eta_g = 1} for all rankers.
#' Higher \eqn{\eta_g = 1}{eta_g = 1} increases the distance between item
#' worths, giving greater weight' to the ranker's choice. Conversely, lower
#' \eqn{\eta_g = 1}{eta_g = 1} shrinks the item worths towards equality so the
#' ranker's choice is less relevant.
#'
#' The adherence parameters are not estimable by maximum likelihood, since
#' for given item worths the maximum likelihood estimate of adherence would be
#' infinity for rankers that give rankings consistent with the items ordered by
#' worth and zero for all other rankers. Therefore it is essential to include a
#' prior on the adherence parameters when these are estimated rather than fixed.
#' Setting \code{gamma = TRUE} specifies the default
#' \eqn{\Gamma(10,10)}{Gamma(10,10)} prior, which has a mean of
#' 1 and a probability of 0.99 that the adherence is between 0.37 and 2.
#' Alternative parameters can be specified by a list with elements \code{shape}
#' and \code{rate}. Setting scale and rate to a common value \eqn{\theta}{theta}
#' specifies a mean of 1; \eqn{\theta \ge}{theta >=} 2 will give low prior
#' probability to near-zero adherence; as \eqn{\theta}{theta} increases the
#' density becomes more concentrated (and more symmetrical) about 1.
#'
#' Since the number of adherence parameters will typically be large and it is
#' assumed the worth and tie parameters are of primary interest, the adherence
#' parameters are not included in model summaries, but are included in the
#' returned object.
#'
#' @section Controlling the fit:
#'
#' For models without priors, using \code{nspseudo = 0} will use standard
#' maximum likelihood, if the network is connected (and throw an error
#' otherwise).
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
#' \href{https://en.wikipedia.org/wiki/Steffensen's_method}{Steffensen's method}.
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
#' For models with priors, the iterative scaling method cannot be used, so BFGS
#' is used by default.
#'
#' @seealso
#'
#' Handling rankings: \code{\link{rankings}}, \code{\link{aggregate}},
#' \code{\link{group}}, \code{\link{choices}},
#' \code{\link{adjacency}}, \code{\link{connectivity}}.
#'
#' Inspect fitted Plackett-Luce models: \code{\link{coef}}, \code{deviance},
#' \code{\link{fitted}}, \code{\link{itempar}}, \code{logLik}, \code{print},
#' \code{\link{qvcalc}}, \code{\link{summary}}, \code{\link{vcov}}.
#'
#' Fit Plackett-Luce tree: \code{pltree}.
#'
#' Example data sets: \code{\link{beans}}, \code{\link{nascar}},
#' \code{\link{pudding}}, \code{\link{preflib}}.
#'
#' Vignette: \code{vignette("Overview", package = "PlackettLuce")}.
#'
#' @note As the maximum tie order increases, the number of possible choices for
#' each rank increases rapidly, particularly when the total number of items is
#' high. This means that the model will be slower to fit with higher \eqn{D}.
#' In addition, due to the current implementation of the `vcov()` method,
#' computation of the standard errors (as by `summary()`) can take almost as
#' long as the model fit and may even become infeasible due to memory limits.
#' As a rule of thumb, for > 10 items and > 1000 rankings, we recommend
#' `PlackettLuce()` for ties up to order 4. For higher order ties, a
#' rank-ordered logit model, see [ROlogit::rologit()] or
#' generalized Mallows Model as in [BayesMallows::compute_mallows()] may be
#' more suitable, as they do not model tied events explicitly.
#'
#' @param rankings a \code{"\link{rankings}"} object, or an object that can be
#' coerced by \code{as.rankings}.  An [`"aggregated_rankings"`][aggregate()]
#' object can be used to specify rankings and weights simultaneously.
#' A \code{"\link{grouped_rankings}"} object should be used when estimating
#' adherence for rankers with multiple rankings per ranker.
#' @param npseudo when using pseudodata: the number of wins and losses to add
#' between each object and a hypothetical reference object.
#' @param normal a optional list with elements named \code{mu} and \code{Sigma}
#' specifying the mean and covariance matrix of a multivariate normal prior on
#' the \emph{log} worths.
#' @param gamma a optional list with elements named \code{shape} and \code{rate}
#' specifying parameters of a gamma prior on adherence parameters for each
#' ranker (use \code{grouped_rankings} to group multiple rankings by ranker).
#' The short-cut \code{TRUE} may be used to specify a Gamma(10, 10) prior. If
#' \code{NULL} (or \code{FALSE}), adherence is fixed to \code{adherence} for
#' all rankers.
#' @param adherence an optional vector of adherence values for each ranker. If
#' missing, adherence is fixed to 1 for all rankers. If \code{gamma} is not
#' \code{NULL}, this specifies the starting values for the adherence.
#' @param weights an optional vector of weights for each ranking.
#' @param na.action a function to handle any missing rankings, see
#' [na.omit()].
#' @param start starting values for the worth parameters and the tie parameters
#' on the raw scale (worth parameters need not be scaled to sum to 1). If
#' \code{normal} is specified, \code{exp(normal$mu)} is used as starting values
#' for the worth parameters. Coefficients from a previous fit can be passed as
#' the result of a call to  \code{coef.PlackettLuce}, or the \code{coefficients}
#' element of a \code{"PlackettLuce"} object.
#' @param method  the method to be used for fitting: \code{"iterative scaling"}
#' (iterative scaling to sequentially update the parameter values),
#' \code{"BFGS"} (the BFGS optimisation algorithm through the
#' \code{\link{optim}} interface), \code{"L-BFGS"} (the limited-memory BFGS
#' optimisation algorithm as implemented in the \code{\link[lbfgs]{lbfgs}}
#' package). Iterative scaling is used by default, unless a prior is specified
#' by \code{normal} or \code{gamma}, in which case the default is \code{"BFGS"}.
#' @param epsilon the maximum absolute difference between the observed and
#' expected sufficient statistics for the ability parameters at convergence.
#' @param steffensen a threshold defined as for \code{epsilon} after which to
#' apply Steffensen acceleration to the iterative scaling updates.
#' @param maxit a vector specifying the maximum number of iterations. If
#' \code{gamma} is \code{NULL}, only the first element is used and specifies the
#' maximum number of iterations of the algorithm specified by \code{method}. If
#' \code{gamma} is not \code{NULL}, a second element may be supplied to specify
#' the maximum number of iterations of an alternating algorithm, where
#' the adherence parameters are updated alternately with the other parameters.
#' The default is to use 10 outer iterations.
#' @param trace logical, if \code{TRUE} show trace of iterations.
#' @param verbose logical, if \code{TRUE} show messages from validity checks on
#' the rankings.
#' @param ... additional arguments passed to \code{optim} or \code{lbfgs}.
#' In particular the convergence tolerance may be adjusted using e.g.
#' \code{control = list(reltol = 1e-10)}.
#'
#' @return An object of class \code{"PlackettLuce"}, which is a list containing
#' the following elements:
#' \item{call}{ The matched call. }
#' \item{coefficients}{ The model coefficients. }
#' \item{loglik}{ The maximized log-likelihood. }
#' \item{null.loglik}{ The maximized log-likelihood for the null model (all
#' alternatives including ties have equal probability). }
#' \item{df.residual}{ The residual degrees of freedom. }
#' \item{df.null}{ The residual degrees of freedom for the null model. }
#' \item{rank}{ The rank of the model. }
#' \item{logposterior}{ If a prior was specified, the maximised log posterior.}
#' \item{gamma}{ If a gamma prior was specified, the list of parameters. }
#' \item{normal}{ If a normal prior was specified, the list of parameters. }
#' \item{iter}{ The number of iterations run. }
#' \item{rankings}{ The rankings passed to \code{rankings}, converted to a
#' \code{"rankings"} object if necessary. }
#' \item{weights}{ The weights applied to each ranking in the fitting. }
#' \item{adherence}{ The fixed or estimated adherence per ranker. }
#' \item{ranker}{ The ranker index mapping rankings to rankers (the
#' \code{"index"} attribute of \code{rankings} if specified as a
#' \code{"grouped_rankings"} object.)}
#' \item{ties}{ The observed tie orders corresponding to the estimated tie
#' parameters. }
#' \item{conv}{ The convergence code: 0 for successful convergence; 1 if reached
#' \code{maxit} (outer) iterations without convergence; 2 if Steffensen
#' acceleration cause log-likelihood to increase; negative number if L-BFGS
#' algorithm failed for other reason.}
#' @references
#' Raman, K. and Joachims, T. (2014)  Methods for Ordinal Peer Grading.
#' \href{https://arxiv.org/abs/1404.3656}{arXiv:1404.3656}.
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
#' # create rankings object
#' R <- as.rankings(R)
#'
#' # Standard maximum likelihood estimates
#' mod_mle <- PlackettLuce(R, npseudo = 0)
#' coef(mod_mle)
#'
#' # Fit with default settings
#' mod <- PlackettLuce(R)
#' # log-worths are shrunk towards zero
#' coef(mod)
#'
#' # independent N(0, 9) priors on log-worths, as in Raman and Joachims
#' prior <- list(mu = rep(0, ncol(R)),
#'               Sigma = diag(rep(9, ncol(R))))
#' mod_normal <- PlackettLuce(rankings = R, normal = prior)
#' # slightly weaker shrinkage effect vs pseudo-rankings,
#' # with less effect on tie parameters (but note small number of rankings here)
#' coef(mod_normal)
#'
#' # estimate adherence assuming every ranking is from a separate ranker
#' mod_separate <- PlackettLuce(rankings = R, normal = prior, gamma = TRUE)
#' coef(mod_separate)
#' # gives more weight to rankers 4 & 6 which rank apple first,
#' # so worth of apple increased relative to banana
#' mod_separate$adherence
#'
#' # estimate adherence based on grouped rankings
#' #  - assume two rankings from each ranker
#' G <- group(R, rep(1:3, each = 2))
#' mod_grouped <- PlackettLuce(rankings = G, normal = prior, gamma = TRUE)
#' coef(mod_grouped)
#' # first ranker is least consistent so down-weighted
#' mod_grouped$adherence
#'
#' @importFrom igraph as_adj graph_from_edgelist
#' @importFrom RSpectra eigs
#' @importFrom stats optim
#' @export
PlackettLuce <- function(rankings,
                         npseudo = 0.5,
                         normal = NULL,
                         gamma = NULL,
                         adherence = NULL,
                         weights = freq(rankings),
                         na.action = getOption("na.action"),
                         start = NULL,
                         method = c("iterative scaling", "BFGS", "L-BFGS"),
                         epsilon = 1e-7, steffensen = 0.1, maxit = c(500, 10),
                         trace = FALSE, verbose = TRUE, ...){
    call <- match.call()

    # check rankings object
    grouped_rankings <- inherits(rankings, "grouped_rankings")
    if (inherits(rankings, "aggregated_rankings")){
        force(weights)
        rankings$freq <- NULL
        rankings <- as.rankings(rankings)
    }
    if (!grouped_rankings & !inherits(rankings, "rankings")){
        rankings <- suppressWarnings(as.rankings(rankings, verbose = verbose))
    }

    # define ranker
    if (grouped_rankings){
        ranker <- attr(rankings, "index")
        # if weights are per group id expand to be per ranking
        if (!is.null(weights) & length(weights) == max(ranker)) {
            weights <- weights[ranker]
        }
    }else if (!is.null(adherence)| !is.null(gamma)){
        ranker <- seq_len(nrow(rankings))
    } else ranker <- NULL

    if (!is.null(na.action)) {
        rankings <- match.fun(na.action)(rankings)
        na.action <- attr(rankings, "na.action")
        if (!is.null(na.action)) {
            if (!is.null(weights)) weights <- weights[-na.action]
            if (!is.null(adherence)) adherence <- adherence[-na.action]
            if (!is.null(ranker)) ranker <- ranker[-na.action]
        }
    }

    # unpack grouped rankings
    if (grouped_rankings){
        R <- attr(rankings, "R")
        S <- attr(rankings, "S")
        id <- attr(rankings, "id")
        rankings <- attr(rankings, "rankings")
    }

    # attributes
    items <- colnames(rankings)
    N <- ncol(rankings) # total number of objects
    nr <- nrow(rankings) # number of rankings

    # weights
    if (is.null(weights)){
        weights <- rep.int(1L, nr)
    }
    stopifnot(length(weights) == nr)
    stopifnot(all(weights > 0L))

    # check gamma prior specification
    if (is.logical(gamma) && length(gamma) == 1L && !is.na(gamma) && !gamma)
        gamma <- NULL # in case specified as FALSE
    if (!is.null(gamma)) { # specified or TRUE
        if (isTRUE(gamma)) gamma <- list(shape = 10, rate = 10)
        stopifnot(names(gamma) == c("shape", "rate"))
        stopifnot(all(lengths(gamma) == 1L))
        if (gamma$shape != gamma$rate)
            warning("mean of adherence prior is not 1")
    }

    # adherence should be per ranker
    if (!is.null(adherence)) {
        stopifnot(length(adherence) == max(ranker))
        stopifnot(all(adherence > 0L))
        a <- adherence[ranker]
    } else if (!is.null(gamma)){
        adherence <- rep.int(1L, max(ranker))
        a <- adherence[ranker]
        wa <- rowsum(weights, ranker)[,1]
    } else a <- NULL

    if (!grouped_rankings){
        # items ranked from last to 1st place
        R <- t(apply(rankings, 1L, order, decreasing = TRUE))
        mode(R) <-"integer"

        # adjacency matrix: wins over rest
        # N.B. need even if `start` specified; used to check connectivity
        X <- adjacency(rankings, weights = weights)

        # sizes of selected sets
        set <- apply(rankings, 1L, function(x){
            last <- which(x == max(x))
            ind <- which(x > 0L)
            # exclude untied last place
            if (length(last) == 1L) ind <- setdiff(ind, last)
            list(size = tabulate(x[ind])[x[ind]], item_id = ind)
        })
        item_id <- unlist(lapply(set, `[[`, "item_id"))
        S <- lapply(set, `[[`, "size")
        ns <- lengths(S)
        ws <- rep.int(weights, ns)
        if (!is.null(adherence)) ranker_id <- rep.int(ranker, ns)
        S <- unlist(S)
        rm(set)
    } else {
        # adjacency matrix: wins over rest
        # (id extracted from grouped_rankings object)
        X <- matrix(0.0, N, N)
        X[sort(unique(unlist(id)))] <-
            rowsum(rep(weights, lengths(id)), unlist(id))
        class(X) <- c("adjacency", "matrix")

        # (S extracted from grouped_rankings object)
        # N.B. here unlist indifferent order than ungrouped
        ws <- (weights * as.logical(S))[as.logical(S)]
        if (!is.null(adherence)) {
            ranker_id <- (ranker * as.logical(S))[as.logical(S)]
        }
        item_id <- R[as.logical(S)]
        S <- S[as.logical(S)]
    }
    # sufficient statistics
    # for delta d, (number of sets with cardinality d)/cardinality
    d <- sort(unique(S))
    D <- length(d)
    B <- rowsum(ws, S)[,1L]
    B <- B/d
    # from now only only need weight/size per set, so replace S with this
    S <- ws/S
    rm(ws)
    # for alpha
    A <- numeric(N)
    item <- sort(unique(item_id))
    if (is.null(adherence)){
        # sum over all sets st object i is in selected set weight/size
        A[item] <- unname(rowsum(S, item_id)[,1L])
    } else {
        # now (adherence * weight)/size
        A[item] <- unname(rowsum(adherence[ranker_id]*S, item_id)[,1L])
    }


    if (!is.null(normal)){
        # set npseudo to 0
        npseudo <- 0L
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
    if (npseudo == 0L & is.null(normal)){
        out <- connectivity(X, verbose = FALSE)
        if (out$no > 1L)
            stop("Network is not fully connected - cannot estimate all ",
                 "item parameters with npseudo = 0")
    }

    # unique rows with >= 1 element for logical matrix
    # case where p is fixed
    uniquerow <- function(M){
        len <- ncol(M)
        if (len == 1L) return(1L)
        pattern <- M[,1L]
        max_exp <- .Machine$double.digits
        for (k in seq_len(len - 1L)){
            # N.B. can't use integer here, can get integer overflow
            pattern <- 2*pattern + M[,k]
            if (k == len - 1L ||
                k %% max_exp == (max_exp - 1L)){
                pattern <- match(pattern, unique(pattern))
                max_exp <- .Machine$double.digits - ceiling(log2(max(pattern)))
            }
        }
        as.integer(pattern)
    }

    # max number of objects in set
    nc <- max(rowSums(rankings > 0L))

    # create W so cols are potential set sizes and value > 0 indicates ranking
    # aggregate common sets
    # - incorporate ranking weights by weighted count vs simple tabulate
    W <- G <- list() # weight (currently rep count); group of rankings
    P <- logical(nc)  # set sizes present in rankings
    minrank <- rep.int(1L, nr)
    for (i in seq_len(nc)){
        p <- (nc - i + 1L)
        set <- rankings >= minrank
        r <- which(rowSums(set) == p)
        P[p] <- p != 1L && length(r)
        if (!P[p]) next
        # separate adherence per ranking
        if (!is.null(gamma) & !grouped_rankings){
            W[[p]] <- weights[r]
            G[[p]] <- r
        } else {
            g <- uniquerow(set[r, , drop = FALSE])
            # combine within ranker (adherence likely different for each ranker)
            if (!is.null(adherence)){
                x <- ranker[r]/10^ceiling(log10(max(ranker[r]))) + g
                g <- match(x, x)
            }
            W[[p]] <- as.vector(unname(rowsum(weights[r], g)))
            G[[p]] <- r[!duplicated(g)]
        }
        minrank[r] <- minrank[r] + 1L
    }
    P <- which(P)

    # if npseudo > 0 add npseudo wins and losses with hypothetical item
    stopifnot(npseudo >= 0L)
    if (npseudo > 0L){
        # update R with paired comparisons with hypothetical item (item N + 1)
        R <- cbind(R, "NULL" = 0L)
        pseudo <- matrix(0L, nrow = N, ncol = N + 1L)
        pseudo[, 1L] <- N + 1L
        pseudo[, 2L]  <- seq_len(N)
        R <- rbind(R, pseudo)
        # update X with npseudo wins and losses  with hypothetical item
        X <- cbind(X, npseudo)
        X <- rbind(X, c(rep.int(npseudo, N), 0L))
        # update adherence: set to 1 for ghost rater
        if (!is.null(adherence)) a <- c(a, rep.int(1L, N))
        # update weights: 2*npseudo comparisons of each pair
        W[[2L]] <- c(W[[2L]], rep.int(2L*npseudo, N))
        # update indices
        G[[2L]] <- c(G[[2L]], (nr + 1L):(nr + N))
        # add set size of 2 to observed set sizes, if necessary
        P <- unique(c(2L, P))
        # update A: npseudo wins for item N + 1 against each other item;
        # npseudo wins for other items
        A <- c(A + npseudo, N*npseudo)
        # update B: 2*npseudo untied choices per item
        B[1L] <- B[1L] + 2L*npseudo*N
    }

    if (is.null(start)){
        if (!is.null(normal)) {
            alpha <- exp(normal$mu)
        } else {
            # (scaled, un-damped) PageRank on underlying paired comparisons
            ncv <- min(nrow(X), 10L)
            alpha <- drop(abs(eigs(X/colSums(X), 1L,
                                   opts = list(ncv = ncv))$vectors))
        }
        delta <- c(1.0, rep.int(0.1, D - 1L))
    } else {
        # if not "coef.PlackettLuce" object, assume on raw scale
        # (with ability of hypothetical item 1), i.e. as returned coefficients)
        if (!length(start) == N + D - 1)
            stop(paste("`start` does not specify enough values for", N, "item",
                       paste("and", D - 1, "tie parameters")[D > 1]))
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
        delta <- c(1.0, delta)
        if (npseudo > 0L) {
            # set alpha for hypothetical item to 1 as in original fit
            if (inherits(start, "coef.PlackettLuce")){
                alpha <- c(alpha, 1.0)
            } else {
                # set to geometric mean as neutral position
                alpha <- c(alpha, exp(mean(log(alpha))))
            }
        }
    }
    if (npseudo) {
        N <- N + 1L
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
                           control = c(eval(dts$control),
                                       list(maxit = maxit[1L]))),
                      dts[setdiff(names(dts), "control")]))
        }
    }

    if (method == "L-BFGS"){
        if (trace) message("No trace implemented for L-BFGS")
        opt <- function(par, obj, gr, ...){
            lbfgs::lbfgs(obj, gr, par, invisible = 1L,
                         max_iterations = maxit[1L], ...)
        }
    }

    # Optimization of log-worths and log-tie parameters via
    obj_common <- function(par) {
        alpha <- exp(par[1L:N])
        delta <- exp(par[-c(1L:N)])
        # assign to parent environment so can use further quantities in score
        assign("fit", expectation("all", alpha, c(1.0, delta),
                                  a, N, d, P, R, G, W),
               envir = parent.env(environment()))
        -loglik_common(c(alpha, delta), N, normal$mu, Rinv, A, B, fit)
    }
    gr_common <- function(par) {
        alpha <- exp(par[1L:N])
        delta <- exp(par[-c(1L:N)])
        -score_common(c(alpha, delta), N, normal$mu, Kinv, A, B, fit) *
            c(alpha, delta)
    }

    # Optimization of log-adherence (for non-ghost raters) via
    obj_adherence <- function(par){
        adherence <- exp(par)
        # assign to parent environment so can use further quantities in score
        assign("fit", normalization(alpha, c(1.0, delta),
                                    adherence[ranker], d, P, R, G, W),
               envir = parent.env(environment()))
        -loglik_adherence(adherence, gamma$shape, gamma$rate, wa, Z, fit)
    }
    gr_adherence <- function(par) {
        adherence <- exp(par)
        -score_adherence(adherence, ranker, gamma$shape, gamma$rate,
                         wa, Z, fit) * adherence
    }

    logp <- NULL
    if (method != "iterative scaling"){
        i <- 0L
        conv <- 1L
        if (!is.null(gamma)){
            mode(maxit) <- "integer"
            if (length(maxit) != 2L) maxit <- c(maxit, 10L)
        }
        repeat{
            # fit model with fixed adherence (a)
            res <- opt(log(c(alpha, delta[-1L])), obj_common, gr_common, ...)

            if (i == maxit[2L] | is.null(gamma)) break
            if (!is.null(gamma)){
                # update alpha, delta & sufficient statistics for adherence
                alpha <- exp(res$par[1L:N])
                delta <- c(1.0, exp(res$par[-(1L:N)]))
                Z <- unname(rowsum(S*log(alpha[item_id]), ranker_id)[,1L])
                if (i > 0L) logp_old <- logp
                # log-posterior after worth update
                logp <- -res$value + sum(wa*((gamma$shape - 1L)*log(adherence) -
                                                 gamma$rate*adherence))
                if (i > 0L && abs(logp_old - logp) <=
                    epsilon * (abs(logp) + epsilon)) {
                    conv <- 0L
                    break
                }
                i <- i + 1L

                # fit model with fixed worth/tie parameters
                # ignore rankings involving ghost item (adherence fixed to 1)
                res2 <- opt(log(adherence), obj_adherence, gr_adherence, ...)

                # update adherence & sufficient stats for alpha (real items)
                adherence <- exp(res2$par)
                a[1L:nr] <- adherence[ranker]
                A[item] <- unname(rowsum(adherence[ranker_id]*S, item_id)[,1L])
                if (npseudo) A[item] <- A[item] + npseudo
                # log-posterior after gamma update
                # -res2$value + sum(B[-1]*log(delta)) -
                #    0.5*tcrossprod((log(alpha) - normal$mu) %*% Rinv)[1]
            }
        }
        if (!is.null(gamma)){
            iter <- i
        } else {
            conv <- res$convergence
            iter <- res$counts # NULL for L-BFGS
        }
        res <- list(alpha = exp(res$par[1L:N]),
                    delta = c(1.0, exp(res$par[-(1L:N)])),
                    logl = -res$value)
    } else {
        res <- list(alpha = alpha, delta = delta)
        res[c("expA", "expB", "theta")] <-
            expectation("all", alpha, delta, a, N, d, P, R, G, W)
        res$logl <- sum(B[-1L]*log(res$delta)[-1L]) + sum(A*log(res$alpha)) -
            sum(res$theta)
        oneUpdate <- function(res){
            # update all alphas
            res$alpha <- res$alpha*A/res$expA
            if (npseudo) res$alpha <- res$alpha/res$alpha[N]
            # update all deltas
            if (D > 1L) {
                res$delta[-1L] <- B[-1L]/
                    expectation("delta", res$alpha, res$delta,
                                a, N, d, P, R, G, W)$expB
            }
            res[c("expA", "expB", "theta")] <-
                expectation("all", res$alpha, res$delta,
                            a, N, d, P, R, G, W)
            res$logl <- sum(B[-1L]*log(res$delta)[-1L]) +
                sum(A*log(res$alpha)) - sum(res$theta)
            res
        }
        accelerate <- function(p, p1, p2){
            # only accelerate if parameter has changed in last iteration
            z <- p2 != p1
            p2[z] <- p[z] - (p1[z] - p[z])^2L / (p2[z] - 2L * p1[z] + p[z])
            p2
        }
        # stopping rule: compare observed & expected sufficient stats
        checkConv <- function(res){
            eps <- abs(c(A, B[-1L]) - c(res$expA, res$delta[-1L]*res$expB))
            assign("eps", eps, envir = parent.env(environment()))
            ifelse(all(eps < epsilon), 0L, 1L)
        }
        iter <- 0L
        if ((conv <- checkConv(res)) == 0L) maxit <- 0L
        updateIter <- function(res){
            if (trace) message("iter ", iter, ", loglik: ", res$logl)
            assign("iter", iter + 1L,
                   envir = parent.env(environment()))
        }
        eps <- c(A, B[-1L])
        doSteffensen <- FALSE
        while(iter < maxit[1L]){
            updateIter(res)
            res <- oneUpdate(res)
            if ((conv <- checkConv(res)) == 0L) break
            if (all(eps < steffensen & !doSteffensen)) doSteffensen <- TRUE
            # steffensen
            if (doSteffensen){
                res1 <- oneUpdate(res)
                if ((conv <- checkConv(res1)) == 0L) {
                    res <- res1
                    break
                }
                res2 <- oneUpdate(res1)
                if ((conv <- checkConv(res2)) == 0L) {
                    res <- res2
                    break
                }
                # if negative worth or log-likelihood decreased,
                # don't apply Steffensen
                res$alpha <-
                    accelerate(res$alpha, res1$alpha, res2$alpha)
                if (all(res$alpha > 0L)) {
                    res$delta[-1L] <-
                        accelerate(res$delta, res1$delta, res2$delta)[-1L]
                    res[c("expA", "expB", "theta")] <-
                        expectation("all", res$alpha, res$delta,
                                    a, N, d, P, R, G, W)
                    res$logl <-
                        sum(B[-1L]*log(res$delta)[-1L]) +
                        sum(A*log(res$alpha)) - sum(res$theta)
                    if (res$logl < res2$logl) {
                        res <- res2
                    } else if ((conv <-  checkConv(res)) == 0L) break
                } else res <- res2
            }
        }
        if (trace) message("iter ", iter, ", loglik: ", res$logl)
    }
    if (conv[1L] == 1L) warning("Iterations have not converged.")

    res$delta <- structure(res$delta, names = paste0("tie", names(B)))[-1L]

    if (npseudo > 0L) {
        # drop hypothetical object
        res$alpha <- res$alpha[-N]
        N <- N - 1L
        # drop weights and indices for pseudodata
        i <- seq_len(length(W[[2L]]) - N)
        W[[2L]] <- W[[2L]][i]
        G[[2L]] <- G[[2L]][i]
        if (!length(W[[2L]])) P <- setdiff(P, 2L)
        # remove contribution to A and B
        A <- A[-(N + 1L)] - npseudo
        B[1L] <- B[1L] - 2L*npseudo*N
    }
    names(res$alpha) <- items
    rank <- N + D - 2L

    cf <- c(res$alpha, res$delta)

    # save normal prior info
    if (!is.null(normal)) {
        normal_prior <- list(mu = normal$mu, invSigma = Kinv)
    } else normal_prior <- NULL

    # recompute log-likelihood excluding pseudo-observations/priors
    if (npseudo > 0L | !is.null(normal) | !is.null(gamma)) {
        if (!is.null(normal) & is.null(gamma)) {
            # logp not yet assigned
            logp <- res$logl
        }
        normal <- NULL
        logl <- -obj_common(log(cf))
    } else logl <- res$logl
    # null log-likelihood - set adherence to 1 if estimated
    if (!is.null(gamma)) a <- rep.int(1L, length(a))
    null.loglik <- -obj_common(rep.int(0L, length(cf)))

    # frequencies of sets selected from, for sizes 2 to max observed
    freq <- vapply(W[P], sum, 1.0)
    # number of possible selections overall
    n <- sum(vapply(P, choose, numeric(D), k = d) %*% freq)
    df.residual <- n - sum(freq) - rank

    fit <- list(call = call,
                coefficients = cf,
                loglik = logl,
                null.loglik = null.loglik,
                df.residual = df.residual,
                df.null = n - sum(freq), #naming consistent with glm
                rank = rank,
                logposterior = logp,
                gamma = gamma,
                normal = normal_prior,
                iter = iter,
                rankings = rankings,
                weights = weights,
                adherence = adherence,
                ranker = ranker,
                ties = d,
                conv = conv,
                na.action = na.action)
    class(fit) <- "PlackettLuce"
    fit
}
