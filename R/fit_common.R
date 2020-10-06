# fitting functions for Plackett-Luce model with adherence fixed ---------------

# Design loglik as brglm2::brglmFit
# log-likelihood and score functions
# Within optim or nlminb use obj and gr wrappers  defined in main function

# full logposterior when estimating adherence (may not have prior on mu)
logp_full <- function(alpha, delta, adherence,
                      mu, Rinv, shape, rate, wa, A, B, theta){
    res <- sum(B[-1L]*log(delta)) + sum(A*log(alpha))- sum(theta)
    # prior on mu
    if (!is.null(mu)){
        # -0.5 * (s - mu)^T Sigma^{-1} (s - mu) + standard logL
        res <- res - 0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1L]
    }
    # prior on adherence - always if estimating adherence
    if (!is.null(shape)){
        res <- res + sum(wa*((shape - 1L)*log(adherence) - rate*adherence))
    }
    res
}

score_full <- function(alpha, delta, adherence, ranker,
                       mu, Kinv, shape, rate, wa, A, B, Z, expA, expB, score){
    a <- A/alpha - expA/alpha
    b <- B[-1L]/delta - expB
    if (length(adherence)){
        z <- Z - rowsum(score, ranker)[, 1L]
        # prior on adherence
        if (!is.null(shape)){
            z <- z + wa*((shape - 1L)/adherence - rate)
        }
    } else z <- numeric(0)
    # prior on log-worth
    if (!is.null(mu)){
        a <- a - Kinv %*% (log(alpha) - mu)/alpha
    }
    c(a, b, z)
}

# omit following constants from log-likelihood/log-posterior:
# contribution from tie in numerator: sum(B[-1]*log(delta))
# contribution from normal prior: -0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1]
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

# compute expectations of the sufficient statistics of the alphas/deltas
# (for fixed adherence) if ranking weight is NULL, do not aggregate rankings
expectation <- function(par, # par to compute expectations for
                        alpha, # alpha par (worth)
                        delta, # delta par (tie)
                        a, # adherence for each ranking
                        N, # number of objects
                        d, # observed tie orders
                        P, # set sizes in representative rankings
                        R, # items in each ranking, from last to first place
                        G, # group of rankings to include; list for each P
                        W = NULL){ # weight of rankings; list for each P
    D <- length(d)
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
        e <- d[d <= p][-1]
        if (length(e)){
            if (keepDelta)
                y1 <- matrix(0.0, nrow = nr, ncol = length(e))
            # index up to max(e) items: start with 1:n
            i <- seq_len(max(e))
            # id = index to change next; id2 = first index changed
            if (max(e) == p) {
                id <- p - 1L
            } else id <- max(e)
            id2 <- 1L
            repeat{
                # work along index vector from 1 to end/first index that = p
                v1 <- alpha[R[r, i[1L]]] # ability for first ranked item
                last <- i[id] == p
                if (last) {
                    end <- id
                } else end <- min(max(e), id + 1L)
                for (k in 2L:end){
                    # product of first k alphas indexed by i
                    v1 <- v1 * alpha[R[r, i[k]]]
                    # ignore if already recorded/tie order not observed
                    if (k < id2 | ! k %in% d) next
                    # add to numerators/denominators for sets of order p
                    if (!is.null(a)) {
                        v2 <- v1^(a[r]/k)
                    } else v2 <- v1^(1L/k)
                    v3 <- delta[d == k]*v2
                    if (keepAlpha) {
                        # add to numerators for objects in sets
                        x1[, i[1L:k]] <- x1[, i[1L:k]] + v3/k
                    }
                    if (keepDelta) {
                        # add to numerator for current tie order for sets
                        y1[, which(d == k) - 1L] <-
                            y1[, which(d == k) - 1L] + v2
                    }
                    # add to denominators for sets
                    z1 <- z1 + v3
                }
                # update index
                if (i[1L] == (p - 1L)) break
                if (last){
                    id2 <- id - 1L
                    v <- i[id2]
                    len <- min(p - 2L - v, max(e) - id2)
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
        if (keepDelta && length(e)){
            if (!is.null(W)){
                expB[seq_along(e)] <- expB[seq_along(e)] +
                    colSums(W[[p]] * y1/z1)
            } else expB[r, seq_along(e)] <- expB[r, seq_along(e)] + y1/z1
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

