# utility functions to create ranking data structures/X matrix for log-linear
# models maybe just for testing or at some point could be tidied up and exported

# okay to aggregate for vcov computation, but not for logLik
#' @importFrom methods cbind2
#' @importFrom utils combn
#' @importFrom Matrix sparseMatrix
poisson_rankings <- function(rankings, weights = NULL,
                             coefs = NULL, adherence = NULL, ranker = NULL,
                             gamma = NULL, aggregate = TRUE,
                             as.data.frame = FALSE){
    # get choices and alternatives for each ranking
    choices <- choices(rankings, names = FALSE)
    # include free choices only
    size <- lengths(choices$alternatives)
    free <- size != 1L
    choices <- lapply(choices, `[`,  free)
    if (!is.null(weights)) {
        choices$w <- weights[choices$ranking]
    } else choices$w <- rep.int(1L, length(choices$ranking))
    if (aggregate && is.null(adherence)) {
        # id unique choices
        unique_choices <- unique(choices$choices)
        g <- match(choices$choices, unique_choices)
        # id unique alternatives
        unique_alternatives <- unique(choices$alternatives)
        h <- match(choices$alternatives, unique_alternatives)
        # count unique choice:alternative combinations
        g <- paste(g, h, sep = ":")
        g <- match(g, unique(g))
        choices$ranking <- split(choices$ranking, g)
        keep <- !duplicated(g)
        agg <- c("choices", "alternatives")
        choices[agg] <- lapply(choices[agg], function(x) x[keep])
        choices$n <- tabulate(g)
        choices$w <- as.numeric(rowsum(choices$w[keep], g[keep]))
        size <- lengths(unique_alternatives)
    } else {
        choices$n <- 1L
        size <- lengths(choices$alternatives)
    }

    # now create X matrix for (unique) alternative sets
    S <- unique(size)
    D <- max(lengths(choices$choices))
    N <- ncol(rankings)
    items <- n <- val <- list()
    for (s in S){
        # generic choices from set of size s
        x <- min(s, D)
        comb <- list()
        for (d in seq_len(x)){
            comb[[d]] <- combn(seq_len(s), d)
        }
        id <- which(size == s)
        n[id] <- list(rep(seq_len(x), vapply(comb, ncol, 1.0)))
        if (aggregate){
            items[id] <- lapply(unique_alternatives[id], `[`, unlist(comb))
        } else items[id] <- lapply(choices$alternatives[id], `[`, unlist(comb))
    }
    # choice id
    x <- sequence(lengths(n))
    # alternative id
    if (aggregate){
        z <- rep(seq_along(unique_alternatives), lengths(n))
    } else z <- rep(seq_along(choices$alternatives), lengths(n))
    # columns for item parameters
    size <- unlist(n)
    A <- sparseMatrix(j = unlist(items), p = c(0L, cumsum(size)),
                      x = rep(1L/size, size))
    ord <- order(A@i)
    all_choices <- split(unlist(items), A@i[ord])
    # columns for tie parameters
    if (D > 1L){
        nr <- nrow(rankings)
        tied <- list()
        for (d in seq_len(D - 1L)){
            tied[[d]] <- which(size == (d + 1L))
        }
        B <- sparseMatrix(i = unlist(tied), p = c(0L, cumsum(lengths(tied))))
        X <- cbind2(A, B)
    } else X <- A
    # update X matrix if extimating adherence (nonlinear model)
    if (!is.null(adherence)){
        a <- adherence[ranker][choices$ranking[z]]
        X[, seq(N)] <- a * X[, seq(N)] # not tie columns
        if (!is.null(coefs)) { # i.e. adherence is estimated vs fixed
            lambda <- coefs[seq_len(N)]
            # columns for adherence parameters
            X2 <- sparseMatrix(i = seq_along(z),
                               j = ranker[choices$ranking[z]],
                               x = X[, seq_len(N)] %*% lambda)
            X <- cbind(X, X2)
        }
    }
    # counts
    if (aggregate){
        alt <- match(choices$alternatives, unique_alternatives)
    } else alt <- seq_along(choices$alternatives)
    na <- length(alt)
    id <- numeric(na)
    row <- split(seq_along(z), z)
    for (i in seq_len(na)){
        ch <- match(choices$choices[i], all_choices[row[[alt[i]]]])
        id[i] <- row[[alt[i]]][ch]
    }
    y <- numeric(length(z))
    y[id] <- choices$n
    if (aggregate){
        w <- numeric(length(z))
        w[id] <- choices$w
    } else w <- choices$w[z]
    if (as.data.frame) {
        dat <- data.frame(y = y)
        dat$X <- as.matrix(X)
        dat$z <- as.factor(z)
        dat$w <- w
        if (!is.null(gamma)) dat$a <- as.factor(ranker[choices$ranking][z])
        dat
    } else {
        res <- list(y = y, X = X, z = z, w = w)
        if (!is.null(gamma)) res$a <- ranker[choices$ranking][z]
        res
    }
}

## A quick way to generate arbitrary ranking data to experinment with
## The larger tie is the lower the chance of a tie is
#' @importFrom stats runif
generate_rankings <- function(maxi, n_rankings = 10L, tie = 5L, seed = NULL) {
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
    mat <- replicate(n_rankings, {
        s <- sample(maxi, maxi)
        m <- sample(maxi, 1L)
        s <- s[s <= m]
        v <- numeric(maxi)
        v[sample(maxi, min(m, maxi))] <- s
        v0 <- v == 0L
        if (length(v0))
            v[v0] <- sample(0L:max(s), sum(v0), replace = TRUE,
                            prob = c(tie, rep(1L/max(s), max(s))))
        v
    })
    as.rankings(t(mat))
}
