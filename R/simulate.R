#' Simulate from \code{PlackettLuce} fitted objects
#'
#' @inheritParams stats::simulate
#'
#' @param multinomial use multinomial sampling anyway? Default is
#'     \code{FALSE}. see Details.
#' @param max_combinations a positive number. Default is
#'     \code{20000}. See Details.
#' @param seed an object specifying if and how the random number
#'     generator should be initialised. Either \code{NULL} or an
#'     integer that will be used in a call to \code{set.seed} before
#'     simulating the rankings. If set, the value is saved as the
#'     \code{seed} attribute of the returned value.  The default,
#'     \code{NULL}, will not change the random generator state, and
#'     return \code{.Random.seed} as the \code{seed} attribute.
#'
#' @details
#'
#' If \code{multinomial} is \code{FALSE} (default) and there are no
#' tie parameters in the object (i.e. \code{length(object$ties) == 1}),
#' then rankings are sampled by ordering exponential random variates
#' with rate 1 scaled by the estimated item-worth parameters
#' \code{object$coefficients} (see, Diaconis, 1988, Chapter 9D for
#' details).
#'
#' In all other cases, the current implementation uses direct
#' multinomial sampling, and will throw an error if there are more
#' than \code{max_combinations} combinations of items that the sampler
#' has to decide from. This is a hard-coded exit to prevent issues
#' relating to the creation of massive objects in memory.
#'
#' If \code{length(object$ties) > 1} the user's setting for
#' \code{multinomial} is ignored and \code{simulate.PlackettLuce} operates as if
#' \code{multinomial} is \code{TRUE}.
#'
#' @return A \code{data.frame} of \code{\link{rankings}} objects of the same
#'     dimension as \code{object$rankings}.
#'
#' @references
#'
#' Diaconis (1988). \emph{Group Representations in Probability and
#' Statistics}. Institute of Mathematical Statistics Lecture Notes
#' 11. Hayward, CA.
#'
#' @examples
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' mod <- PlackettLuce(R)
#' simulate(mod, 5)
#'
#' s1 <- simulate(mod, 3, seed = 112)
#' s2 <- simulate(mod, 2, seed = 112)
#'
#' identical(s1[1:2], s2[1:2])
#'
#' @importFrom stats rexp runif simulate
#' @method simulate PlackettLuce
#' @export
simulate.PlackettLuce <- function(object, nsim = 1L, seed = NULL,
                                  multinomial = FALSE,
                                  max_combinations = 2e+04, ...) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1L)
    if (is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    rankings <- unclass(object$rankings)
    N <- ncol(rankings)
    n_rankings <- nrow(rankings)
    id <- seq(length(object$coefficients) - length(object$ties) + 1L)
    alpha <- object$coefficients[id]
    delta <- numeric(max(object$ties))
    delta[object$ties] <- c(1.0, unname(object$coefficients[-id]))
    opt <- seq_len(N)
    sets <- as.list(numeric(n_rankings))
    for (i in seq_len(n_rankings)) {
        sets[[i]] <- opt[rankings[i,] > 0L]
    }
    len <- lengths(sets)

    ## If there are no ties use Louis Gordon (1983, Annals of Stat);
    ## Diaconis (1988, Chapter 9D)
    if (length(object$ties) == 1L & !multinomial) {
        sampler <- function(objects) {
            v <- numeric(N)
            len <- length(objects)
            ordering <- objects[order(rexp(len, rate = 1L)/alpha[objects],
                                      decreasing = FALSE)]
            v[ordering] <- seq.int(len)
            v
        }
    }
    else {
        ## NOTE, IK 10/12/2017: This is an inefficient computation
        ## with potentially massive objects FIX, IK 10/12/2017: Remove
        ## dependence on all combinations. For now a preventive stop
        ## if n_combinations is more than max_combinations
        n_combinations <- sum(choose(N, object$ties))
        if (n_combinations > max_combinations) {
            stop(paste("simulate.PlackettLuce needs to decide between",
                       n_combinations,
                       "combinations of items, but 'max_combinations' is",
                       max_combinations))
        }

        ## Get all possible combinations of objects
        combinations <- NULL
        for (j in object$ties) {
            combinations <- c(combinations, combn(opt, j, simplify = FALSE))
        }

        ## Unormalized probabilities of all combinations
        probs <- vapply(combinations, function(z) {
            delta[length(z)] * prod(alpha[z])^(1L/length(z))
        }, 1)
        ## NOTE, IK 10/12/2017: Normalization is done internally by sample.int
        sampler <- function(objects) {
            v <- numeric(N)
            j <- 1L
            indices <- rep(TRUE, n_combinations)
            while (length(objects)) {
                ## find out which combinations have all of their objects
                ## included in `objects`
                indices[indices] <- vapply(combinations[indices], function(x) {
                    all(x %in% objects)
                }, TRUE)
                ## sample, after setting the probability of all other
                ## combinations to zero
                sampled <- combinations[[sample.int(n_combinations, 1L,
                                                    prob = probs * indices)]]
                ## remove the sampled objects from `objects`
                objects <- objects[-match(sampled, objects, nomatch = 0L)]
                v[sampled] <- j
                j <- j + 1L
            }
            v
        }
    }

    out <- replicate(nsim, {
        R <- t(vapply(sets, sampler, numeric(N)))
        colnames(R) <- colnames(rankings)
        as.rankings(R)
    }, simplify = FALSE)
    out <- data.frame(out)
    names(out) <- paste("sim", seq_len(nsim), sep = "_")
    attr(out, "seed") <- RNGstate
    out
}


## TODO, IK, 10/12/2017:
## * Handling of weights
## * Avoid constructing all combinations
## * Exploit ideas for multinomial sampling with massive number of categories
## * Add parallelization capabilities
