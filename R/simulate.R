#' Simulate from \code{PlackettLuce} fitted objects
#'
#' @inheritParams stats::simulate
#'
#' @param multinomial use multinomial sampling anyway? Default is \code{FALSE}. see Details.
#' @param max_items a positive number (default is 10). See Details.
#'
#' @details
#'
#' The current implementation will throw an error if more than
#' \code{max_items} (default is 10) items are present in the
#' \code{PlackettLuce} object, when \code{multinomial = TRUE} and/or
#' \code{object$maxTied > 1}. This is a hard-coded condition to
#' prevent issues relating to the creation of massive objects in
#' memory.
#'
#' If \code{object$maxTied > 1} then \code{multinomial} is taken as
#' \code{TRUE}.
#'
#' @return An object of class \code{\link{rankings}} of the same
#'     dimension as \code{object$rankings}
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
simulate.PlackettLuce <- function(object, nsim = 1, seed = NULL, multinomial = FALSE, max_items = 10, ...) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)
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
    n_choices <- 2^N - 1
    id <- seq(length(object$coefficients) - object$maxTied + 1)
    alpha <- object$coefficients[id]
    delta <- numeric(N)
    delta[seq_len(object$maxTied)] <- c(1, unname(object$coefficients[-id]))
    opt <- seq_len(N)
    sets <- as.list(numeric(n_rankings))
    for (i in seq_len(n_rankings)) {
        sets[[i]] <- opt[rankings[i,] > 0]
    }
    len <- lengths(sets)

    ## If there are no ties use Louis Gordon (1983, Annals of Stat); Diaconis (1988, Chapter 9D)
    if (object$maxTied == 1 & !multinomial) {
        sampler <- function(objects) {
            v <- numeric(N)
            len <- length(objects)
            ordering <- objects[order(rexp(len, rate = 1)/alpha[objects], decreasing = FALSE)]
            v[ordering] <- seq.int(len)
            v
        }
    }
    else {
        ## NOTE, IK 10/12/2017: This is an inefficient computation with potentially massive objects
        ## FIX, IK 10/12/2017: Remove dependence on all combinations. For now
        ## a preventive stop if more than max_items objects have been ranked
        if (any(len > max_items)) {
            stop(paste("detected more than", max_items, "items per ranking; current implementation of simulate.PlackettLuce is not apropriate for large number of items"))
        }
        ## Get all possible combinations of objects
        combinations <- NULL
        for (j in seq_len(max(len))) {
            combinations <- c(combinations, combn(opt, j, simplify = FALSE))
        }
        ## Unormalized probabilities of all combinations
        probs <- sapply(combinations, function(z) delta[length(z)] * prod(alpha[z])^(1/length(z)))
        ## NOTE, IK 10/12/2017: Normalization is done internally by sample.int
        sampler <- function(objects) {
            v <- numeric(N)
            j <- 1
            indices <- rep(TRUE, n_choices)
            while (length(objects)) {
                ## find out which combinations have all of their objects included in `objects`
                indices[indices] <- sapply(combinations[indices], function(x) {
                    all(x %in% objects)
                })
                ## sample, after setting the probability of all other combinations to zero
                sampled <- combinations[[sample.int(n_choices, 1, prob = probs * indices)]]
                ## remove the sampled objects from `objects`
                objects <- objects[-match(sampled, objects, nomatch = 0)]
                v[sampled] <- j
                j <- j + 1
            }
            v
        }
    }

    out <- replicate(nsim, {
        R <- t(sapply(sets, sampler))
        colnames(R) <- colnames(rankings)
        as.rankings(R)
    }, simplify = FALSE)
    names(out) <- paste("sim", seq_len(nsim), sep = "_")
    out
}


## TODO, IK, 10/12/2017:
## * Handling of weights
## * Avoid constructing all combinations
## * Exploit ideas for multinomial sampling with massive number of categories
## * Add parallelization capabilities
