vcov_hessian <- function(object){
    rankings <- object$rankings
    weights <- object$weights
    adherence <- object$adherence
    ranker <- object$ranker
    if (!is.null(adherence)){
        a <- adherence[ranker]
        wa <- rowsum(weights, ranker)[,1]
    } else a <- NULL
    gamma <- object$gamma
    normal <- object$normal

    # attributes
    items <- colnames(rankings)
    N <- ncol(rankings) # total number of objects
    nr <- nrow(rankings) # number of rankings

    # items ranked from last to 1st place
    R <- t(apply(rankings, 1L, order, decreasing = TRUE))
    mode(R) <-"integer"

    # sizes of selected sets
    set <- apply(rankings, 1L, function(x){
        last <- which(x == max(x))
        ind <- which(x > 0L)
        # exclude untied last place
        if (length(last) == 1L) ind <- setdiff(ind, last)
        list(size = tabulate(x[ind])[x[ind]], item_id = ind)
    })
    item_id <- unlist(lapply(set, `[[`, "item_id"))
    S <- lapply(set, `[[`, "size")
    ns <- lengths(S)
    w <- rep.int(weights, ns)
    if (!is.null(object$adherence)) ranker_id <- rep.int(ranker, ns)
    S <- unlist(S)
    rm(set)

    # sufficient statistics
    # for delta d, (number of sets with cardinality d)/cardinality
    d <- sort(unique(S))
    D <- length(d)
    B <- unname(rowsum(w, S)[,1L])
    B <- B/d
    # from now only only need weight/size per set, so replace S with this
    S <- w/S
    rm(w)
    # for alpha
    A <- numeric(N)
    item <- sort(unique(item_id))
    if (is.null(adherence)){
        # sum over all sets st object i is in selected set weight/size
        A[item] <- unname(rowsum(S, item_id)[,1L])
    } else {
        # now (adherence * weight)/size
        A[item] <- unname(rowsum(adherence[ranker_id]*S, item_id)[,1L])
    }

    if (!is.null(object$normal)){
        # compute inverse of variance covariance matrix (used in score function)
        K <- solve(object$normal$invSigma)
        Rinv <- solve(chol(K))
        Kinv <- Rinv %*% t(Rinv)
    } else Rinv <- Kinv <- NULL

    # unique rows with >= 1 element for logical matrix
    # case where p is fixed
    uniquerow <- function(M){
        len <- ncol(M)
        if (len == 1L) return(1L)
        pattern <- M[,1L]
        max_exp <- .Machine$double.digits
        for (k in seq_len(len - 1L)){
            # N.B. can't use integer here, can get integer overflow
            pattern <- 2*pattern + M[,k]
            if (k == len - 1L ||
                k %% max_exp == (max_exp - 1L)){
                pattern <- match(pattern, unique(pattern))
                max_exp <- .Machine$double.digits - ceiling(log2(max(pattern)))
            }
        }
        as.integer(pattern)
    }

    # max number of objects in set
    nc <- max(rowSums(rankings > 0L))

    # create W so cols are potential set sizes and value > 0 indicates ranking
    # aggregate common sets
    # - incorporate ranking weights by weighted count vs simple tabulate
    W <- G <- list() # weight (currently rep count); group of rankings
    P <- logical(nc)  # set sizes present in rankings
    minrank <- rep.int(1L, nr)
    for (i in seq_len(nc)){
        p <- (nc - i + 1L)
        set <- rankings >= minrank
        r <- which(rowSums(set) == p)
        P[p] <- p != 1L && length(r)
        if (!P[p]) next
        g <- uniquerow(set[r, , drop = FALSE])
        if (!is.null(adherence)){ # combine within ranker (adherence may change)
            x <- ranker[r]/log10(max(ranker[r]) + 1L) + g
            g <- match(x, x)
        }
        W[[p]] <- as.vector(unname(rowsum(weights[r], g)))
        G[[p]] <- r[!duplicated(g)]
        minrank[r] <- minrank[r] + 1L
    }
    P <- which(P)

    # objective for computing vcov minus the full log-posterior
    obj_full <- function(par){
        alpha <- exp(par[1L:N])
        delta <- exp(par[((N + 1L):(N + D))[-D]])
        adherence <- exp(par[-(1L:(N + D - 1L))])
        if (length(adherence)){
            # update adherence per ranking
            a[1L:nr] <- adherence[ranker]
        }
        # assign to parent environment so can use further quantities in score
        assign("fit", PlackettLuce:::expectation("all", alpha, c(1.0, delta),
                                                 a, N, d, P, R, G, W),
               envir = parent.env(environment()))
        # update A
        if (!length(adherence)){
            A[item] <- unname(rowsum(S, item_id)[,1L])
        } else {
            A[item] <- unname(rowsum(adherence[ranker_id]*S, item_id)[,1L])
        }
        - PlackettLuce:::logp_full(alpha, delta, adherence,
                                   normal$mu, Rinv, gamma$shape, gamma$rate,
                                   wa, A, B, fit$theta)
    }
    gr_full <- function(par) {
        alpha <- exp(par[1L:N])
        delta <- exp(par[((N + 1L):(N + D))[-D]])
        adherence <- exp(par[-(1L:(N + D - 1L))])
        if (!is.null(gamma)){
            norm <- normalization(alpha, c(1.0, delta),
                                  adherence[ranker], d, P, R, G, W)
            # update Z
            Z <- unname(rowsum(S*log(alpha[item_id]), ranker_id)[,1L])
        } else Z <- NULL
        -PlackettLuce:::score_full(alpha, delta, adherence, ranker,
                                   normal$mu, Rinv, gamma$shape, gamma$rate,
                                   wa, A, B, Z,
                                   fit$expA, fit$expB, norm$score) *
            c(alpha, delta, adherence)[-c(1L, D)]
    }
    # cannot compute hessian with our gradient function as it relies on
    # objective function being called immediately prior (as in optimisaton)

    alpha <- object$coefficients[seq_len(N)]
    delta <- object$coefficients[((N + 1L):(N + D))[-D]]
    par <- c(log(alpha),  log(delta))
    if (!is.null(adherence)) {
        par <- c(par, log(adherence))
    }
    H <- stats:::optimHess(par, obj_full)
    p <- length(object$coefficients)
    if (is.null(normal)) {
        V <- matrix(0, p, p)
        V[-1L, -1L] <- solve(H[-1,-1])[seq_len(p - 1L), seq_len(p - 1L)]
    } else {
        V <- solve(H)
        M <- diag(nrow(V))
        M[1L:N, 1L] <- M[1L:N, 1L] - 1L
        V <- (M %*% V %*% t(M))[seq_len(p), seq_len(p)]
    }
    V
}

