# fitting functions for Plackett-Luce model with adherence fixed ---------------

# Design loglik as brglm2::brglmFit
# log-likelihood and score functions
# Within optim or nlminb use obj and gr wrappers  defined in main function

# full logposterior when estimating adherence (may not have prior on mu)
logposterior <- function(alpha, delta, adherence,
                         mu, Rinv, shape, rate, A, B, fit){
    res <- sum(B[-1L]*log(delta)) + sum(A*log(alpha))- sum(fit$theta)
    # prior on mu
    if (!is.null(mu)){
        # -0.5 * (s - mu)^T Sigma^{-1} (s - mu) + standard logL
        res <- res - 0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1L]
    }
    # prior on adherence - always if estimating adherence
    res + (shape - 1L)*sum(log(adherence)) - rate*sum(adherence)
}

# omit following constants from log-likelihood/log-posterior:
# contribution from tie in numerator: sum(B[-1]*log(delta))
# contribution from normal prior: - 0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1L]
# normalising constant from gamma prior: shape*log(rate) - log(gamma(shape))
loglik_common <- function(par, N, mu, Rinv, A, B, fit){
    alpha <- par[1L:N]
    delta <- par[-c(1L:N)]
    res <- sum(B[-1L]*log(delta)) + sum(A*log(alpha))- sum(fit$theta)
    if (is.null(mu)) return(res)
    # -0.5 * (s - mu)^T Sigma^{-1} (s - mu) + standard logL
    res - 0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1L]
}

# derivatives of log-likelihood for common parameters (worth, tie)
score_common <- function(par, N, mu, Kinv, A, B, fit) {
    alpha <- par[1L:N]
    delta <- par[-c(1L:N)]
    res <- c(A/alpha - fit$expA/alpha, B[-1L]/delta - fit$expB)
    if (is.null(mu)) return(res)
    # deriv first part wrt alpha (s) : [-Sigma^{-1} (s - mu)]/alpha
    res[1L:N] <- res[1L:N] - Kinv %*% (log(alpha) - mu)/alpha
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
    keepDelta <- D > 1L && any(par %in% c("delta", "all"))
    keepTheta <- any(par %in% c("theta", "all"))
    expA <- expB <- theta <- NULL
    if (keepAlpha) {
        if (!is.null(W)) {
            expA <- numeric(N)
        } else expA <- matrix(0.0, nrow = nrow(R), ncol = N)
    }
    if (keepDelta) {
        if (!is.null(W)) {
            expB <- numeric(D - 1L)
        } else expB <- matrix(0.0, nrow = nrow(R), ncol = D - 1L)
    }
    if (keepTheta) theta <- numeric(sum(lengths(G[P])))
    z <- 1L
    for (p in P){
        # D == 1
        ## numerators (for expA, else just to compute denominators)
        r <- G[[p]]
        nr <- length(r)
        item_id <- as.integer(R[r, 1L:p])
        x1 <- matrix(alpha[item_id],
                     nrow = nr, ncol = p)
        if (!is.null(a)) x1 <- x1^a[r]
        ## denominators
        z1 <- rowSums(x1)
        # D > 1
        d <- min(D, p)
        if (d > 1L){
            if (keepDelta)
                y1 <- matrix(0.0, nrow = nr, ncol = d - 1L)
            # index up to d items: start with 1:n
            i <- seq_len(d)
            # id = index to change next; id2 = first index changed
            if (d == p) {
                id <- p - 1L
            } else id <- d
            id2 <- 1L
            repeat{
                # work along index vector from 1 to end/first index = s
                v1 <- alpha[R[r, i[1L]]] # ability for first ranked item
                last <- i[id] == p
                if (last) {
                    end <- id
                } else end <- min(d, id + 1L)
                for (k in 2L:end){
                    # product of first k alphas indexed by i
                    v1 <- v1 * alpha[R[r, i[k]]]
                    # ignore if already recorded
                    if (k < id2) next
                    # add to numerators/denominators for sets of order s
                    if (!is.null(a)) {
                        v2 <- v1^(a[r]/k)
                    } else v2 <- v1^(1L/k)
                    v3 <- delta[k]*v2
                    if (keepAlpha) {
                        # add to numerators for objects in sets
                        x1[, i[1L:k]] <- x1[, i[1L:k]] + v3/k
                    }
                    if (keepDelta) {
                        # add to numerator for current tie order for sets
                        y1[, k - 1L] <- y1[, k - 1L] + v2
                    }
                    # add to denominators for sets
                    z1 <- z1 + v3
                }
                # update index
                if (i[1L] == (p - 1L)) break
                if (last){
                    id2 <- id - 1L
                    v <- i[id2]
                    len <- min(p - 2L - v, d - id2)
                    id <- id2 + len
                    i[id2:id] <- v + seq_len(len + 1L)
                } else {
                    id2 <- id
                    i[id] <- i[id] + 1L
                }
            }
        }
        # add contribution for sets of size s to expectation
        if (keepAlpha){
            # R[r, 1:s] may only index some alphas
            if (!is.null(a)) x1 <- a[r] * x1
            if (!is.null(W)){
                id <- unique(item_id)
                add <- drop(rowsum(as.vector(W[[p]] * x1/z1),
                                   item_id, reorder = FALSE))
                expA[id] <- expA[id] + add
            } else {
                id <- cbind(r, item_id)
                expA[id] <- expA[id] + c(x1/z1)
            }
        }
        if (keepDelta && p > 1L){
            if (!is.null(W)){
                expB[seq_len(d - 1L)] <- expB[seq_len(d - 1L)] +
                    colSums(W[[p]] * y1/z1)
            } else expB[r, seq_len(d - 1L)] <- expB[r, seq_len(d - 1L)] + y1/z1
        }
        if (keepTheta){
            if (par == "all"){
                # return logtheta
                if (!is.null(W)){
                    theta[z:(z + nr - 1L)] <- W[[p]] * log(z1)
                } else theta[z:(z + nr - 1L)] <- log(z1)
            } else {
                if (!is.null(W)){
                    theta[z:(z + nr - 1L)] <- W[[p]] * z1
                } else theta[z:(z + nr - 1L)] <- z1
            }
            z <- z + nr
        }
    }
    list(expA = if (keepAlpha) expA, expB = if (keepDelta) expB,
         theta = if (keepTheta) theta)
}

