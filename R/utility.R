# utility functions to create ranking data structures/X matrix for log-linear models
# maybe just for testing or at some point could be tidied up and exported

# okay to aggregate for vcov computation, but not for logLik
#' @importFrom utils combn
#' @importFrom Matrix sparseMatrix cBind
poisson_rankings <- function(rankings, weights = NULL, aggregate = TRUE,
                             as.data.frame = FALSE){
    # get choices and alternatives for each ranking
    choices <- choices(rankings, names = FALSE)
    # include free choices only
    size <- lengths(choices$alternatives)
    free <- size != 1
    choices <- lapply(choices, `[`,  free)
    if (!is.null(weights)) {
        choices$w <- weights[choices$ranking]
    } else choices$w <- rep.int(1, length(choices$ranking))
    if (aggregate) {
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
        choices$w <- as.integer(rowsum(choices$w[keep], g[keep]))
        size <- lengths(unique_alternatives)
    } else {
        choices$n <- 1
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
        n[id] <- list(rep(seq_len(x), vapply(comb, ncol, 1)))
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
    A <- sparseMatrix(j = unlist(items), p = c(0, cumsum(size)),
                      x = rep(1/size, size))
    ord <- order(A@i)
    all_choices <- split(unlist(items), A@i[ord])
    # columns for tie parameters
    if (D > 1){
        nr <- nrow(rankings)
        tied <- list()
        for (d in seq_len(D - 1)){
            tied[[d]] <- which(size == (d + 1))
        }
        B <- sparseMatrix(i = unlist(tied), p = c(0, cumsum(lengths(tied))))
        X <- cBind(A, B)
    } else X <- A
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
        dat
    } else list(y = y, X = X, z = z, w = w)
}

## A quick way to generate arbitrary ranking data to experinment with
## The larger tie is the lower the chance of a tie is
generate_rankings <- function(maxi, n_rankings = 10, tie = 5, seed = NULL) {
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
    mat <- replicate(n_rankings, {
        s <- sample(maxi, maxi)
        m <- sample(maxi, 1)
        s <- s[s <= m]
        v <- numeric(maxi)
        v[sample(maxi, min(m, maxi))] <- s
        v0 <- v==0
        if (length(v0))
            v[v0] <- sample(0:max(s), sum(v0), replace = TRUE, prob = c(tie, rep(1/max(s), max(s))))
        v
    })
    as.rankings(t(mat))
}
