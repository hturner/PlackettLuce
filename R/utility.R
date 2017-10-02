# utility functions to create ranking data structures/X matrix for log-linear models
# maybe just for testing or at some point could be tidied up and exported

# basic function to create multinomial outcomes for partial rankings, no ties
# assumes records are items ranked 1st, 2nd, ..., with 0 for no item ranked
longdat <- function(dat){
    N <- max(dat)
    X <- diag(N)
    resX <- resY <- resZ <- list()
    k <- 1
    for (i in 1:nrow(dat)){
        y <- numeric(N)
        nobj <- max(which(dat[i,] != 0))
        resX[[i]] <- resY[[i]] <-resZ[[i]] <- list()
        for(j in 1:(nobj - 1)){
            y[dat[i,j]] <- 1
            ind <- unlist(dat[i, j:nobj])
            resY[[i]][[j]] <- y[ind]
            resX[[i]][[j]] <- X[ind, ]
            resZ[[i]][[j]] <- rep(k, nobj - j + 1)
            k <- k + 1
        }
    }
    res <- data.frame(y = unlist(resY))
    res$X <- do.call("rbind", unlist(resX, recursive = FALSE))
    res$z <- factor(unlist(resZ))
    res
}

# basic function to create multinomial outcomes for partial rankings with ties
# assumes records are dense rankings
#' @importFrom utils combn
longdat2 <- function(R){
    N <- ncol(R)
    D <- max(apply(R, 1, function(x) max(tabulate(x))))
    X <- list()
    for (d in 1:D){
        comb <- combn(1:N, d)
        A <- matrix(0, nrow = ncol(comb), ncol = N)
        A[cbind(rep(1:ncol(comb), each = nrow(comb)), c(comb))] <- 1/d
        B <- matrix(0, nrow = ncol(comb), ncol = D - 1)
        if (ncol(B)) B[, d - 1] <- 1
        X[[d]] <- cbind(A, B)
    }
    resX <- resY <- resZ <- list()
    k <- 1
    for (i in 1:nrow(R)){
        J <- max(R[i,])
        J <- J - (sum(R[i,] == J) == 1)
        resX[[i]] <- resY[[i]] <- resZ[[i]] <- list()
        for (j in seq_len(J)){
            id <- which(R[i,] < j)
            Xij <- list()
            for (d in 1:min(D, N - length(id))){
                keep <- rowSums(X[[d]][, id, drop = FALSE]) == 0
                Xij[[d]] <- X[[d]][keep,]
            }
            resX[[i]][[j]] <- do.call("rbind", Xij)
            resZ[[i]][[j]] <- rep(k, nrow(resX[[i]][[j]]))
            resY[[i]][[j]] <- numeric(nrow(resX[[i]][[j]]))
            id <- colSums(t(resX[[i]][[j]][, 1:N]) == (R[i,] == j)/sum(R[i,] == j)) == N
            resY[[i]][[j]][id] <- 1
            k <- k + 1
        }
    }
    res <- data.frame(y = unlist(resY))
    res$X <- do.call("rbind", unlist(resX, recursive = FALSE))
    res$z <- factor(unlist(resZ))
    res
}

# okay to aggregate for vcov computation, but not for logLik
#' @import Matrix Matrix
poisson_rankings <- function(rankings, aggregate = TRUE, as.data.frame = FALSE){
    # get choices and alternatives for each ranking
    choices <- as.choices(rankings, names = FALSE)
    # include free choices only
    size <- lengths(choices$alternatives)
    free <- size != 1
    choices <- lapply(choices, `[`,  free)
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
        choices$n <- as.integer(table(g))
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
    if (as.data.frame) {
        dat <- data.frame(y = y)
        dat$X <- as.matrix(X)
        dat$z <- as.factor(z)
        dat
    } else list(y = y, X = X, z = z)
}

# convert ranked items to dense rankings for each object
denseRanking <- function(M){
    t(apply(M, 1, function(x) match(1:max(M), x, nomatch = 0)))
}

