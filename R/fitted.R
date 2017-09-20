## Todo:
## Drop dependence on tibble
## Avoid re-evaluation for repeated choices-alternatives combinations
## Optimize subsetting in prob

#' Fitted probabilities for PlackettLuce objects
#'
#' Fitted probabilities for all choice/alternative combinations in the data
#'
#' @param object a \code{"PlackettLuce"} object as returned by
#' \code{\link{PlackettLuce}}.
#' @param ... further arguments passed to method (ignored).
#'
#' @seealso as.choices
#' @importFrom tibble as.tibble
#' @export
fitted.PlackettLuce <- function(object, ...) {
    patterns <- object$patterns
    choices <- as.choices(object$rankings, names = FALSE)
    choices <- as.tibble(choices)
    objects <- attr(choices, "objects")
    choices$D <- unlist(lapply(choices[["choices"]], length))
    choices$T <- unlist(lapply(choices[["alternatives"]], length))
    npatterns <- ncol(patterns)
    nobjects <- nrow(patterns)
    alpha <- object$coefficients[seq_len(nobjects)]
    delta <- c(1, object$coefficients[-seq_len(nobjects)])
    nconst <- object$constants
    prob <- function(obs) {
        D <- obs$D
        alt <- obs$alternatives
        ch <- obs$choices
        if (obs$T == 1) 1 else { delta[D] * prod(alpha[ch])^(1/D) / nconst[colSums(patterns == (seq.int(nobjects) %in% alt)) == nobjects] }
    }
    choices$fitted <- apply(choices, 1, prob)
    choices$D <- choices$T <- NULL
    attr(choices, "objects") <- objects
    choices
}

fittedvalues <- function(object, aggregate = TRUE, ...) {
    # work with matrix of rankings - avoids checking on subsetting of rankings
    # and no names => choices etc are unnamed
    object$rankings <- unname(unclass(object$rankings))
    # get parameters
    id <- seq(length(object$coefficients) - object$maxTied + 1)
    alpha <- object$coefficients[id]
    delta <- c(1, mod$coefficients[-id])
    # recreate S matrix
    S <- apply(object$rankings, 1, sort, decreasing = TRUE)
    S <- as(-1*t(apply(rbind(S, 0), 2, diff)), "dgCMatrix")
    # set sizes present in rankings
    P <- which(colSums(S) > 0)
    N <- max(P)
    # set up objects
    alternatives <- choices <- n <- rankings <- fitted <- list()
    # chunk by number of items in set (start with largest)
    for (i in rev(seq_along(P))){
        p <- P[i]
        # rankings that select from this number of items
        rankings[[i]] <- which(S[, p, drop = FALSE] == 1)
        # aggregate winners and losers
        agg <- aggregate_weights(object$rankings, S, N, p,
                                 method = "winners_and_losers")
        rankings[[i]] <- unname(split(rankings[[i]], agg$g))
        n[[i]] <- tabulate(agg$g)
        # get representative ranking per group to find alternatives and choices
        r <- vapply(rankings[[i]], "[", 1, 1)
        agg$minrank <- agg$minrank[!duplicated(agg$g)]
        ## possibly could use sparse martrix index here vs which
        alternatives[[i]] <- t(apply(object$rankings[r, , drop = FALSE] >=
                                         agg$minrank, 1, which))
        if (p == 1) alternatives[[i]] <- t(alternatives[[i]])
        choices[[i]] <- lapply(seq_along(r), function(i) {
            which(object$rankings[r[i],] == agg$minrank[i])
        })
        # estimate fitted value
        # maybe vectorise by size of choice but keep simple for now!
        numerator <- vapply(choices[[i]], function(ch) {
            n <- length(ch)
            delta[n]*prod(alpha[ch])^(1/n)}, 1)
        denominator <- expectation2("theta", alpha, delta, alternatives[[i]],
                                    S[r, , drop = FALSE],
                                    N = 4, D = object$maxTied, P = p)$theta
        fitted[[i]] <- numerator/denominator
    }
    # tidy up to return
    alternatives <- lapply(alternatives, function(x){
        lapply(seq_len(nrow(x)), function(i) x[i, ])
    })
    res <- data_frame(choices = unlist(choices, recursive = FALSE),
                      alternatives = unlist(alternatives, recursive = FALSE),
                      n = unlist(n),
                      rankings = unlist(rankings, recursive = FALSE),
                      fitted = unlist(fitted))
    if (!aggregate){
        res <- res[rep.int(seq_len(nrow(res)), res$n),
                   c("choices", "alternatives", "rankings", "fitted")]
        res$rankings <- unlist(rankings)
        names(res)[3] <- "ranking"
        res <- res[order(res$ranking, N - lengths(res$alternatives)),]
    }
    res
}
