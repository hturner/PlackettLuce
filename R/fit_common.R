# fitting functions for Plackett-Luce model with adherence fixed ---------------

# Design loglik as brglm2::brglmFit
# log-likelihood and score functions
# Within optim or nlminb use obj and gr wrappers below

# omit following constants from log-likelihood/log-posterior:
# contribution from tie in numerator: sum(B[-1]*log(delta))
# contribution from normal prior: - 0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1]
# normalising constant from gamma prior: shape*log(rate) - log(gamma(shape))
loglik_common <- function(par, N, mu, Rinv, A, B, fit){
    alpha <- par[1:N]
    delta <- par[-c(1:N)]
    res <- sum(B[-1]*log(delta)) + sum(A*log(alpha))- sum(fit$theta)
    if (is.null(mu)) return(res)
    # -0.5 * (s - mu)^T Sigma^{-1} (s - mu) + standard logL
    res - 0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1]
}

# derivatives of log-likelihood for common parameters (worth, tie)
score_common <- function(par, N, mu, Kinv, A, B, fit) {
    alpha <- par[1:N]
    delta <- par[-c(1:N)]
    res <- c(A/alpha - fit$expA/alpha, B[-1]/delta - fit$expB)
    if (is.null(mu)) return(res)
    # deriv first part wrt log alpha (s) : [-1/nobs Sigma^{-1} (s - mu)]
    res[1:N] <- res[1:N] - Kinv %*% (log(alpha) - mu)/alpha
    res
}

# function to compute expectations of the sufficient statistics of the alphas/deltas
# (for fixed adherence) if ranking weight is NULL, do not aggregate across rankings
expectation <- function(par, # par to compute expectations for
                        alpha, # alpha par (worth)
                        delta, # delta par (tie)
                        a, # adherence for each ranking
                        N, # number of objects
                        D, # max tie order
                        P, # set sizes in representative rankings
                        R, # items in each ranking, from last to first place
                        G, # group of rankings to include; list for each P
                        W = NULL){ # weight of rankings; list for each P
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
    if (keepTheta) theta <- numeric(sum(lengths(G[P])))
    z <- 1
    for (p in P){
        # D == 1
        ## numerators (for expA, else just to compute denominators)
        r <- G[[p]]
        nr <- length(r)
        x1 <- matrix(alpha[R[r, 1:p]],
                     nrow = nr, ncol = p)
        if (!is.null(a)) x1 <- x1^a[r]
        ## denominators
        z1 <- rowSums(x1)
        # D > 1
        d <- min(D, p)
        if (d > 1){
            if (keepDelta)
                y1 <- matrix(0, nrow = nr, ncol = d - 1)
            # index up to d items: start with 1:n
            i <- seq_len(d)
            # id = index to change next; id2 = first index changed
            if (d == p) {
                id <- p - 1
            } else id <- d
            id2 <- 1
            repeat{
                # work along index vector from 1 to end/first index = s
                v1 <- alpha[R[r, i[1]]] # ability for first ranked item
                last <- i[id] == p
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
                if (i[1] == (p - 1)) break
                if (last){
                    id2 <- id - 1
                    v <- i[id2]
                    len <- min(p - 2 - v, d - id2)
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
            if (!is.null(a)) x1 <- a[r] * x1
            if (!is.null(W)){
                id <- unique(as.integer(R[r, 1:p]))
                add <- drop(rowsum(as.vector(W[[p]] * x1/z1),
                                   c(R[r, 1:p]), reorder = FALSE))
                expA[id] <- expA[id] + add
            } else {
                id <- cbind(r, c(R[r, 1:p]))
                expA[id] <- expA[id] + c(x1/z1)
            }
        }
        if (keepDelta && p > 1){
            if (!is.null(W)){
                expB[seq_len(d - 1)] <- expB[seq_len(d - 1)] +
                    colSums(W[[p]] * y1/z1)
            } else expB[r, seq_len(d - 1)] <- expB[r, seq_len(d - 1)] + y1/z1
        }
        if (keepTheta){
            if (par == "all"){
                # return logtheta
                if (!is.null(W)){
                    theta[z:(z + nr - 1)] <- W[[p]] * log(z1)
                } else theta[z:(z + nr - 1)] <- log(z1)
            } else {
                if (!is.null(W)){
                    theta[z:(z + nr - 1)] <- W[[p]] * z1
                } else theta[z:(z + nr - 1)] <- z1
            }
            z <- z + nr
        }
    }
    list(expA = if (keepAlpha) expA, expB = if (keepDelta) expB,
         theta = if (keepTheta) theta)
}

