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

# convert ranked items to dense rankings for each object
denseRanking <- function(M){
    t(apply(M, 1, function(x) match(1:max(M), x, nomatch = 0)))
}

#' Choices and alternatives
#'
#' coerce a matrix of rankings to a list of choices, alternatives, and
#' rankings. The choices and the corresponding alternatives is the
#' exchangeable bit of the Plackett-Luce with ties.
#'
#' @examples
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' actual_choices <- as.choices(R, names = TRUE)
#' coded_choices <- as.choices(R, names = FALSE)
#' attr(coded_choices, "objects")
#'
#' ## Coercion to tibble is straightforwards
#' tibble::as.tibble(coded_choices)
as.choices <- function(rankings, names = FALSE) {
    N <- ncol(rankings)
    M <- t(Matrix(rankings, sparse = TRUE))
    J <- apply(M, 2, max)
    onames <- colnames(rankings)
    opt <- seq_len(N)
    if (names & !is.null(onames)) {
        opt <- onames
    }
    choices <- alternatives <-  list()
    rankings <- c()
    for (j in seq_len(max(J))) {
        ## j-th choices
        choices <- c(choices, as.list(apply((M == j)[, J >= j, drop = FALSE], 2, function(z) opt[z])))
        ## j-th alternatives
        alternatives <- c(alternatives, as.list(apply((M > j - 1)[, J >= j, drop = FALSE], 2, function(z) opt[z])))
        rankings <- c(rankings, which(J >= j))
    }
    ii <- order(rankings)
    out <- list(choices = choices[ii], alternatives = alternatives[ii], ranking = rankings[ii])
    attr(out, "nchoices") <- length(choices)
    attr(out, "objects") <- matrix(c(seq_len(N), onames), ncol = 2)
    class(out) <- c("choices", class(out))
    out
    ## Alow weights per choice/alternatives combination?
}

print.choices <- function(x, ...) {
    rankings <- x$ranking
    for (i in unique(rankings)) {
        cat("Ranking:", i, "\n")
        cat("-------------- \n")
        ccho <- x$choices[rankings == i]
        calt <- x$alternatives[rankings == i]
        for (j in seq_along(ccho)) {
            ch <- paste0("{", paste(ccho[[j]], collapse = ", "), "}")
            al <- paste0("{", paste(calt[[j]], collapse = ", "), "}")
            cat(ch, "from", al, "\n")
        }
        cat("============== \n")
    }
}
