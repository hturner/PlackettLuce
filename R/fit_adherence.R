# fitting functions for Plackett-Luce model with worth and tie parameters fixed

# Design loglik as brglm2::brglmFit
# log-likelihood and score functions
# Within optim or nlminb use obj and gr defined in main function

# omit following constants from log-likelihood/log-posterior:
# contribution from tie in numerator: sum(B[-1]*log(delta))
# contribution from normal prior: -0.5*tcrossprod((log(alpha) - mu) %*% Rinv)[1]
# normalising constant from gamma prior: shape*log(rate) - log(gamma(shape))
loglik_adherence <- function(adherence, shape, rate, wa, Z, fit) {
    ## Z = sum(average log worth for each choice)
    res <- sum(adherence*Z) - sum(fit$norm)
    # prior on adherence
    if (!is.null(shape)){
        res <- res + sum(wa*((shape - 1L)*log(adherence) - rate*adherence))
    }
    res
}

# derivatives of log-likelihood for adherence
score_adherence <- function(adherence, ranker, shape, rate, wa, Z, fit) {
    res <- Z - rowsum(fit$score, ranker)
    # prior on adherence
    if (!is.null(shape)){
        res <- res + wa*((shape - 1L)/adherence - rate)
    }
    res
}

# compuation of normalization constants and their derivative wrt adherence,
# per ranking
normalization <- function(alpha, # alpha par (worth)
                          delta, # delta par (tie)
                          a, # adherence for each ranking
                          d, # max tie order
                          P, # set sizes in representative rankings
                          R, # items in each ranking, from last to first place
                          G, # group of rankings to include; list for each P
                          W = NULL){ # weight of rankings; list for each P
    nr <- length(a) # number of real rankings
    score <- numeric(nr)
    # ignore any pseudo-rankings (always pairs)
    len <- lengths(G)
    pseudo <- len[2L] > nr
    if (pseudo) {
        real <- G[[2L]] <= nr
        len[2L] <- sum(real)
    }
    norm <- numeric(sum(len))
    z <- 1L
    for (p in P){
        # D == 1
        ## numerator of score
        if (p == 2L && pseudo) {
            r <- G[[p]][real]
        } else r <- G[[p]]
        nr <- length(r)
        item_id <- as.integer(R[r, 1L:p])
        x1 <- matrix(alpha[item_id],
                     nrow = nr, ncol = p)
        y1 <- rowSums(x1^a[r]*log(x1))
        ## denominator of score = exp(contribution to norm constant)
        z1 <- rowSums(x1^a[r])
        # D > 1
        e <- d[d <= p][-1]
        if (length(e)){
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
                    v2 <- v1^(a[r]/k)
                    y1 <- y1 + delta[d == k]*v2*log(v1)/k
                    # add to denominators for sets
                    z1 <- z1 + delta[d == k]*v2
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
        # add contribution for sets of size s to norm constant/scores
        if (!is.null(W)){
            if (p == 2L && pseudo) {
                w <- W[[p]][real]
            } else w <- W[[p]]
            norm[z:(z + nr - 1L)] <- norm[z:(z + nr - 1L)] + w * log(z1)
            score[r] <- score[r] + w * y1/z1
        } else {
            norm[z:(z + nr - 1L)] <- norm[z:(z + nr - 1L)] + log(z1)
            score[r] <- score[r] + y1/z1
        }
        z <- z + nr
    }
    list(norm = norm, score = score)
}
