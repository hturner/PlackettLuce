# fitting functions for Plackett-Luce model with worth and tie parameters fixed

# Design loglik as brglm2::brglmFit
# log-likelihood and score functions
# Within optim or nlminb use obj and gr defined in main function

# omit following constants from log-likelihood/log-posterior:
# contribution from tie in numerator: sum(B[-1]*log(delta))
# contribution from normal prior: - 0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1]
# normalising constant from gamma prior: shape*log(rate) - log(gamma(shape))
loglik_adherence <- function(adherence, shape, rate, Z, fit) {
    ## Z = sum(average log worth for each choice)
    res <- sum(adherence*Z) - sum(fit$norm)
    # prior on adherence
    if (!is.null(shape)){
        res <- res + (shape - 1L)*sum(log(adherence)) - rate*sum(adherence)
    }
    res
}

# derivatives of log-likelihood for adherence
score_adherence <- function(adherence, ranker, shape, rate, Z, fit) {
    res <- Z - rowsum(fit$score, ranker)
    # prior on adherence
    if (!is.null(shape)){
        res <- res + (shape - 1L)/adherence - rate
    }
    res
}

# compuation of normalization constants and their derivative wrt adherence,
# per ranking
normalization <- function(alpha, # alpha par (worth)
                          delta, # delta par (tie)
                          a, # adherence for each ranking
                          D, # max tie order
                          P, # set sizes in representative rankings
                          R, # items in each ranking, from last to first place
                          G, # group of rankings to include; list for each P
                          W = NULL){ # weight of rankings; list for each P
    score <- numeric(length(a))
    norm <- numeric(sum(lengths(G[P])))
    z <- 1L
    for (p in P){
        # D == 1
        ## numerator of score
        r <- G[[p]]
        if (p == 2L) r <- r[r <= length(a)] # ignore pseudo-rankings
        nr <- length(r)
        x1 <- matrix(alpha[R[r, 1L:p]],
                     nrow = nr, ncol = p)
        y1 <- rowSums(x1^a[r]*log(x1))
        ## denominator of score = exp(contribution to norm constant)
        z1 <- rowSums(x1^a[r])
        # D > 1
        d <- min(D, p)
        if (d > 1L){
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
                    v2 <- v1^(a[r]/k)
                    y1 <- y1 + delta[k]*v2*log(v1)/k
                    # add to denominators for sets
                    z1 <- z1 + v2
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
        # add contribution for sets of size s to norm constant/scores
        if (!is.null(W)){
            norm[z:(z + nr - 1L)] <- norm[z:(z + nr - 1L)] + W[[p]] * log(z1)
            score[r] <- score[r] + W[[p]] * y1/z1
        } else {
            norm[z:(z + nr - 1L)] <- norm[z:(z + nr - 1L)] + log(z1)
            score[r] <- score[r] + W[[p]] * y1/z1
        }
        z <- z + nr
    }
    list(norm = norm, score = score)
}
