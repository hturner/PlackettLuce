# R port of selected utils functions from utils.py as here:
# https://github.com/neu-spiral/FastAndAccurateRankingRegressionFunctions

# Global variables coverted to default arguments

# Initialization, start from a feasible point for all parameters
init_params <- function(X, orderings,  mat_Pij, method_beta_b_init = "QP",
                        rtol = 1e-4){
    # n: number of items
    # p: number of features
    # param orderings: (c_l, A_l): 1...M
    # param X: n*p, feature matrix
    # param mat_Pij = est_Pij(n, orderings)
    # Q = sum for all pairs (i, j):
    # [(P_ij x_j - P_ji x_i); (P_ij - P_ji)][(P_ij x_j - P_ji x_i); (P_ij - P_ji)]^T
    n <- dim(X)[1]
    if (method_beta_b_init == 'LS'){
        beta_b <- init_beta_b_ls(X = X, orderings = orderings, rtol = rtol)
    } else {
        beta_b <- init_beta_b_convex_QP(X = X, orderings = orderings,
                                        mat_Pij = mat_Pij, rtol = rtol)
    }
    ## weights are initialized uniformly as in all other methods
    start_pi <- Sys.time()
    pi_init <- rep.int(1/n, n)
    time_pi_init <- Sys.time() - start_pi
    ## u is initialized
    start_u <- Sys.time()
    u_init <- numeric(n)
    time_u_init <- Sys.time() - start_u
    ## theta newton is initialized
    start_theta <- Sys.time()
    theta_init <- numeric(n)
    time_theta_init <- Sys.time() - start_theta
    ## beta newton exp beta is initialized
    exp_beta <- init_exp_beta(X, orderings, mat_Pij)
    list(beta_init = beta_b$beta_init, b_init = beta_b$b_init,
         time_beta_b_init = beta_b$time_beta_b_init,
         pi_init = pi_init, time_pi_init = time_pi_init,
         u_init = u_init, time_u_init = time_u_init,
         theta_init = theta_init, time_theta_init = time_theta_init,
         exp_beta_init = exp_beta$exp_beta_init,
         time_exp_beta_init = exp_beta$time_exp_beta_init)
}

init_beta_b_ls <- function(X, orderings, rtol = 1e-4){
    # least squares initialization for parameter vector beta and bias b
    # n: number of items
    # p: number of features
    # param orderings: (c_l, A_l): 1...M
    # param X: n*p, feature matrix
    # param rtol: convergence tolerance
    # return: beta, b, time
    ## beta is initialized to minimize least squares on the labels,
    ## not the scores: (y_ij-(x_i-x_j)^T beta)^2
    sum_cross_corr <- 0
    sum_auto_corr <- 0
    m <- nrow(orderings)
    n <- ncol(orderings) # assume orderings of same length
    for (r in seq_len(m)){
        for (i in seq_len(n)){
            winner <- orderings[r, i]
            for (loser in orderings[r, -seq_len(i)]){
                X_ij <- X[winner, ] - X[loser, ]
                sum_cross_corr <- sum_cross_corr + X_ij
                sum_auto_corr <- sum_auto_corr + outer(X_ij, X_ij)
            }
        }
    }
    start_beta_b <- Sys.time()
    beta_init <- solve(sum_auto_corr, sum_cross_corr )
    ## b is initialized so that X*beta + b is positive in the beginning
    x_beta_init <- X %*% beta_init
    b_init <- max(-x_beta_init) + rtol
    time_beta_b_init <- Sys.time() - start_beta_b
    list(beta_init = beta_init, b_init = b_init,
         time_beta_b_init = time_beta_b_init)
}

#' @importFrom CVXR Variable Minimize Problem quad_form solve
init_beta_b_convex_QP <- function(X, orderings, mat_Pij, rtol = 1e-4){
    # n: number of items
    # p: number of features
    # :param orderings: (c_l, A_l): 1...M
    # :param X: n*p, feature matrix
    # :param mat_Pij = est_Pij(n, orderings)
    # param: rtol: convergence tolerance
    # min._{beta,b} {beta,b}^T Q {beta,b}, s.t.
    # Xbeta + b >=0 and sum(Xbeta + b)=1
    # :return: beta, b, time
    p <- ncol(X) + 1
    # Define variables
    params <- Variable(p) # c(beta, b)
    # Define objective
    Q <- est_sum_dij_dijT(X, orderings, mat_Pij)
    objective <- Minimize(quad_form(params, Q))
    # Define constraints
    constraints <- list(X %*% params[-p] + params[p] >= rtol, # X %*% beta + b
                        sum(X %*% params[-p] + params[p]) == 1)
    start_beta_b <- Sys.time()
    # Optimize
    prob <- Problem(objective, constraints = constraints)
    res <- solve(prob, solver = "SCS") # splitting conic solver
    time_beta_b_init <- Sys.time() - start_beta_b
    if (is.null(res$value)) {# or res$status != "optimal" ?
        constraints <- list(X %*% params[-p] + params[p] >= rtol)
        # If cannot be solved, reduce the accuracy requirement
        start_beta_b <- Sys.time()
        # Optimize
        prob <- Problem(objective, constraints = constraints)
        res <- solve(prob, solver = "SCS", eps = 1e-2)
        time_beta_b_init <- Sys.time() - start_beta_b
    }
    params <- res$getValue(params)
    list(beta_init = params[-p], b_init = params[p],
         time_beta_b_init = time_beta_b_init)
}

est_sum_dij_dijT <- function(X, orderings, mat_Pij = NULL){
    # n: number of items
    # p: number of features
    # :param orderings: (c_l, A_l): 1...M
    # :param X: n*p, feature matrix
    # :param mat_Pij = est_Pij(n, orderings)
    # :return: sum for all pairs (i, j):
    # [(P_ij x_j - P_ji x_i); (P_ij - P_ji)][(P_ij x_j - P_ji x_i); (P_ij - P_ji)]^T
    n <- nrow(X)
    p <- ncol(X)
    if (is.null(mat_Pij)){
        mat_Pij <- est_Pij(n, orderings)
    }
    sum_dij_dijT <- array(0, dim = c(p+1, p+1))
    items <- seq_len(n)
    for (i in items){
        for (j in items){
            if (i != j){
                d_ij <- c(mat_Pij[i, j] * X[j, ] - mat_Pij[j, i] * X[i, ],
                          mat_Pij[i, j] - mat_Pij[j, i])
                sum_dij_dijT <- sum_dij_dijT  + outer(d_ij, d_ij)
            }
        }
    }
    sum_dij_dijT
}

est_Pij <- function(n, orderings){
    # n: number of items
    # p: number of features
    # :param orderings: (c_l, A_l): 1...M
    # :param X: n*p, feature matrix
    # :return: for each pair (i, j), empirical estimate of Prob(i beats j)
    # Python original uses list of lists sparse matrix for constructing then
    # converts to compressed sparse column matrix - will only be sparse for
    # large number of items, use dense for now
    Pij <- array(0, dim = c(n, n))
    m <- nrow(orderings)
    p <- ncol(orderings) # assume orderings of same length
    # count pairwise wins in each choice
    for (r in seq_len(m)){
        for (i in seq_len(n)){
            winner <- orderings[r, i]
            for (loser in orderings[r, -seq_len(i)]){
                Pij[winner, loser] <-  Pij[winner, loser] + 1
            }
        }
    }
    # turn into pairwise probability of win/loss
    copy_Pij <- Pij
    for (i in seq_len(n)){
        for (j in i:n){
            summation <- copy_Pij[i, j] + copy_Pij[j, i]
            if (summation > 0){
                Pij[i, j] <- Pij[i, j]/summation
                Pij[j, i] <- Pij[j, i]/summation
            }
        }
    }
    Pij
}

init_exp_beta <- function(X, orderings, mat_Pij = NULL){
    # least squares initialization for beta in exponential parametrization
    # (alternative: exp_beta_init = np.ones((p,), dtype=float) * epsilon)
    # n: number of items
    # p: number of features
    # :param orderings: (c_l, A_l): 1...M
    # :param X: n*p, feature matrix
    # :param mat_Pij = est_Pij(n, orderings)
    # :return: exp_beta, time
    n <- nrow(X)
    if (is.null(mat_Pij)){
        mat_Pij <- est_Pij(n, orderings)
    }
    sum_auto_corr <- 0
    sum_cross_corr <- 0
    items <- ncol(orderings)
    for (i in seq_len(items)){
        for (j in seq_len(items)){
            if (i != j){
                X_ij <- X[i, ] - X[j, ]
                sum_auto_corr <- sum_auto_corr + outer(X_ij, X_ij)
                if (mat_Pij[i, j] > 0 && mat_Pij[j, i] > 0){
                    s_ij <- mat_Pij[i, j] / mat_Pij[j, i]
                } else if (mat_Pij[i, j] == 0 && mat_Pij[j, i] == 0){
                    s_ij <- 1
                } else if (mat_Pij[i, j] == 0 && mat_Pij[j, i] > 0){
                    s_ij <- 1/nrow(orderings)
                } else if (mat_Pij[i, j] > 0 && mat_Pij[j, i] == 0){
                    s_ij <- nrow(orderings)
                }
                sum_cross_corr <- sum_cross_corr + log(s_ij) * X_ij
            }
        }
    }
    start_exp_beta <- Sys.time()
    exp_beta_init <- solve(sum_auto_corr, sum_cross_corr)
    time_exp_beta_init <- Sys.time() - start_exp_beta
    list(exp_beta_init = exp_beta_init,
         time_exp_beta_init = time_exp_beta_init)
}

softmax <- function(a){
    tmp <- exp(a - max(a))
    tmp / sum(tmp)
}

#' @importFrom Matrix lu
statdist <- function(generator, method = "power", v_init = NULL, n_iter = 500,
                     rtol = 1e-4){
    # Compute the stationary distribution of a Markov chain,
    # described by its infinitesimal generator matrix.
    # Computing the stationary distribution can be done with one of the
    # following methods:
    # - `kernel`: directly computes the left null space (co-kernel) the
    # generator matrix using its LU-decomposition. Alternatively:
    # ns = spl.null_space(generator.T)
    # - `eigenval`: finds the leading left eigenvector of an equivalent
    # discrete-time MC using `scipy.sparse.linalg.eigs`.
    # - `power`: finds the leading left eigenvector of an equivalent
    # discrete-time MC using power iterations. v_init is the initial eigenvector
    n <- nrow(generator)
    if (method == "kernel"){
        # `lu` contains U on the upper triangle, including the diagonal.
        res <- Matrix::lu(t(generator))
        lu <- matrix(res@x, n, n)
        # The last row contains 0's only.
        left <- lu[-n, -n]
        right <- -lu[-n, n]
        # Solves system `left * x = right`. Assumes that `left` is
        # upper-triangular (ignores lower triangle.)
        res <- backsolve(left, right)
        res <- c(res, 1L)
        return(res/sum(res))
    }
    if (method == "eigenval"){
        # Arnoldi iteration has cubic convergence rate, but does not guarantee
        # positive eigenvector
        # mat = generator+eye is row stochastic, i.e. rows add up to 1.
        # Multiply by eps to make sure each entry is at least -1. Sum by 1 to
        # make sure that each row sum is exactly 1.
        eps <- 1.0 / max(abs(generator))
        mat <- diag(n) + eps*generator
        A <- t(mat)
        # Find the leading left eigenvector, corresponding to eigenvalue 1
        res <- eigen(A)$vectors[,1]
        return (res / sum(res))
    }
    if (method == "power"){
        # Power iteration has linear convergence rate and slow for
        # lambda2~lambda1.
        # But guarantees positive eigenvector, if started accordingly.
        if (is.null(v_init)){
            v <- runif(n)
        } else {
            v <- v_init
        }
        # mat = generator+eye is row stochastic, i.e. rows add up to 1.
        # Multiply by eps to make sure each entry is at least -1.
        # Sum by 1 to make sure that each row sum is exactly 1.
        eps <- 1L / max(abs(generator))
        mat <- diag(n) + eps * generator
        A <- t(mat)
        # Find the leading left eigenvector, corresponding to eigenvalue 1
        normAest <- sqrt(max(rowSums(abs(A))) * max(colSums(abs(A))))
        v <- v/norm(v)
        Av <- A %*% v
        for (ind_iter in seq_len(n_iter)){
            v <- Av/norm(Av)
            Av <- A %*% v
            lamda <- c(t(v) %*% Av)
            r <- Av - v*lamda
            normr <- norm(r)
            if (normr < rtol*normAest){
                #print('Power iteration converged in ' +
                #       str(ind_iter) + ' iterations.')
                break
            }
        }
        res <- Re(v)
        return (res/sum(res))
    } else {
        stop("not (yet?) implemented")
    }
}

objective <- function(weights, orderings, epsilon = .Machine$double.eps){
    ll <- 0
    n <- ncol(orderings)
    # against log(0), add epsilon
    for (r in seq_len(nrow(orderings))){
        nitem <- sum(orderings[r,] != 0)
        for (i in seq_len(nitem)){
            sum_weights <- sum(weights[orderings[r, i:n]])
            winner <- orderings[r, i]
            ll <- ll + log(weights[[winner]] + epsilon) -
                log(sum_weights + epsilon)
        }
    }
    ll
}

# Euclidean 2-norm (sqrt(sum(x^2))) avoiding over/underflow
# c.f. https://stackoverflow.com/a/63763823/173755
norm <- function(x){
    m <- abs(max(x))
    m*sqrt(sum((x/m)^2))
}
