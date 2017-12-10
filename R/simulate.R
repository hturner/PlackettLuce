## TODO Add handling of grouped_rankings

#' Simulate from PlackettLuce objects
#'
#'
#'
simulate.PlackettLuce <- function(object, nsim = 1, seed = NULL, ...) {
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

        rankings <- unclass(object$rankings)
        N <- ncol(rankings)
        id <- seq(length(object$coefficients) - object$maxTied + 1)
        alpha <- object$coefficients[id]
        delta <- numeric(N)
        delta[seq_len(object$maxTied)] <- c(1, unname(object$coefficients[-id]))

        opt <- seq_len(N)
        sets <- apply(rankings, 1, function(x) opt[x > 0])

        ## Get all possible combinations of objects
        combinations <- NULL
        for (j in opt) {
            combinations <- c(combinations, combn(opt, j, simplify = FALSE))
        }
        ## Unormalized probabilities
        probs <- sapply(combinations, function(z) delta[length(z)] * prod(alpha[z])^(1/j))
        n_choices <- length(probs)

        ## NOTE, IK 10/12/2017: Normalization will be done internally by sample.int
        ## NOTE, IK 10/12/2017: Can be written as recursion but while neat it will be slower
        sampler <- function(objects) {
            v <- numeric(N)
            j <- 1
            while (length(objects)) {
                indices <- sapply(combinations, function(x) {
                    all(x %in% objects)
                })
                sampled <- combinations[[sample.int(n_choices, 1, prob = probs * indices)]]
                objects <- objects[-match(sampled, objects, nomatch = 0)]
                v[sampled] <- j
                j <- j + 1
            }
            v
        }

        out <- replicate(nsim, {
            R <- t(sapply(sets, sampler))
            colnames(R) <- colnames(rankings)
            as.rankings(R)
        }, simplify = FALSE)
        names(out) <- paste("sim", seq_len(nsim), sep = "_")
        out
}


