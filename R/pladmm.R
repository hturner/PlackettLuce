# wrapper function to fit Plackett-Luce model with PLADMM-log, based on Python
# code in run_methods.py as here:
# https://github.com/neu-spiral/FastAndAccurateRankingRegressionFunctions
#' @export
pladmm <- function(rankings, # rankings object as used in PlackettLuce
                   X, # model matrix
                   rho = 1, # penalty parameter
                   n_iter = 500, # used for main iter, pi update & stationary dist
                   rtol = 1e-4, # used in convergence checks: main iter, init of beta (& pi if QP init used), pi update & stationary dist
                   epsilon = .Machine$double.eps # added to pi avoid logging zero
                   ){
    # convert dense rankings to orderings as used in original Python functions
    rankings <- as.matrix(rankings)
    orderings <- t(apply(rankings, 1, order))
    # pairwise probability of win/loss
    mat_Pij <- est_Pij(nrow(X), orderings)

    # initialization of parameters
    inits <-  init_params(X, orderings, mat_Pij, method_beta_b_init = "QP")

    # quantities to update in iterations
    log_admm <- ADMM_log$new(orderings, X, method_pi_tilde_init = "prev")
    conv <- FALSE
    beta_iter <- inits$exp_beta_init #uses init from init_exp_beta!
    ## set b_iter so that exp(X*beta_iter + b_iter) sum to 1
    lambda <- X %*% beta_iter
    b_iter <- -log(sum(exp(lambda)))
    pi_iter <- inits$pi_init
    u_iter <- inits$u_init
    time <- inits$time_exp_beta_init + inits$time_u_init
    diff_pi <- norm(pi_iter)
    diff_beta <- norm(beta_iter)
    prim_feas <- norm(X %*% beta_iter + b_iter - log(pi_iter + epsilon))
    dual_feas <- norm(t(cbind(X, 1)) %*% log(pi_iter + epsilon))
    obj <- objective(pi_iter, orderings)
    iter <- 0

    # iterative updates
    p <- ncol(X)
    for (iter in seq_len(n_iter)){
        # log_admm update
        if (!conv){
            pi_prev <- pi_iter
            beta_prev <- beta_iter
            tilde_pi_prev <- exp(X %*% beta_iter + b_iter)

            res <- log_admm$fit_log(rho = rho, weights = pi_iter,
                                    beta = beta_iter, b = b_iter, u = u_iter)
            pi_iter <- res$weights
            beta_iter <- res$beta
            b_iter <- res$b
            u_iter <- res$u
            time_iter <- res$time
            # scores predicted by beta
            tilde_pi_iter <- exp(X %*% beta_iter + b_iter)
            time <- c(time, time_iter)
            diff_pi <- c(diff_pi, norm(pi_prev - pi_iter))
            diff_beta <- c(diff_beta, norm(beta_prev - beta_iter))
            prim_feas <- c(prim_feas,
                           norm(X %*% beta_iter + b_iter - log(pi_iter + epsilon)))
            dual_feas <- c(dual_feas,
                           norm(t(cbind(X, 1)) %*% (log(pi_prev + epsilon) - log(pi_iter + epsilon))))
            obj <- c(obj, objective(pi_iter, orderings, epsilon))
            iter <- iter + 1
            conv <- norm(pi_prev - pi_iter) < rtol * norm(pi_iter) &&
                norm(tilde_pi_prev - tilde_pi_iter) < rtol * norm(tilde_pi_iter)
        }
        # stop if converged
        if (conv) {
            break
        }
    }

    # cumulate iteration times
    time_cont <- vapply(seq_along(time),
                        function(ind) sum(time[1:ind]), numeric(1))

    list(# parameters from last iteration
         pi = c(pi_iter), beta = c(beta_iter), b = b_iter,
         u = c(u_iter), tilde_pi = c(tilde_pi_iter),
         # stored information from all iterations
         time = time_cont, diff_pi = diff_pi, diff_beta = diff_beta,
         prim_feas = prim_feas, dual_feas = dual_feas, obj = obj,
         # algorithm status
         iter = iter, conv = conv)
}
