# R port of ADMM_log class from admm_log.py as here:
# https://github.com/neu-spiral/FastAndAccurateRankingRegressionFunctions

ADMM_log <- R6::R6Class("ADMM_log",
                    public = list(
                        n = NA,
                        p = NA,
                        M = NA,
                        X = NA,
                        X_ls = NA,
                        X_tilde = NA,
                        orderings = NA,
                        method_pi_tilde_init = NA,
                        initialize = function(orderings, X,
                                              method_pi_tilde_init = "prev"){
                            # n: number of items
                            # p: number of features
                            # M: number of orderings
                            # :param orderings: (c_l, A_l): 1...M
                            # :param X: n*p, feature matrix
                            # :param method_pi_tilde_init: for ilsr_feat, initialize with prev_weights or orthogonal projection
                            self$n <- nrow(X)
                            self$p <- ncol(X)
                            self$M <- nrow(orderings)
                            self$X <- X
                            self$X_ls <- solve(crossprod(X), t(X))
                            self$X_tilde <- cbind(X, 1)
                            self$orderings <- orderings
                            self$method_pi_tilde_init = method_pi_tilde_init
                        },
                        fit_log = function(rho, weights = NULL,
                                           beta = NULL, b = NULL,
                                           u = NULL, gamma = 1,
                                           epsilon = .Machine$double.eps){
                            # :param rho: penalty parameter
                            # :param beta: parameter vector at each iteration, px1
                            # :param weights: scores at each iteration, nx1
                            # :param u: scaled dual variable at each iteration, nx1
                            # :param gamma: scaling on the dual variable update
                            if (is.null(beta)){
                                params <- init_params(self$X, self$orderings,
                                                      mat_Pij = NULL)
                                beta <- params$exp_beta_init
                                # fix b so sum(exp(X %*% beta)) = 1
                                b_iter <- -log(sum(exp(self$X %*% beta)))
                            }
                            start <- Sys.time()
                            ## pi update
                            x_beta <- self$X_tilde %*% c(beta, b)
                            weights <- self$ilsrx_log(rho = rho, weights = weights,
                                                      x_beta = x_beta, u = u,
                                                      epsilon = epsilon)
                            ## dual update
                            u <- u + gamma * (x_beta - log(weights + epsilon))
                            ## beta_b update
                            # beta = spl.lstsq(self.X, np.log(weights) - u)[0]  # uses svd
                            X_ls <- solve(crossprod(self$X_tilde), t(self$X_tilde)) # store in self later
                            beta_b <- X_ls %*% (log(weights + epsilon) - u)
                            end <- Sys.time()
                            list(weights = weights, beta = beta_b[-(self$p + 1)],
                                 b = beta_b[self$p + 1], u = u,
                                 time = (end - start))
                        },
                        ilsrx_log = function(rho, weights, x_beta, u,
                                             n_iter = 500,
                                             rtol = 1e-4,
                                             epsilon = .Machine$double.eps){
                            # modified spectral ranking algorithm for partial ranking data. Remove the inner loop for top-1 ranking.
                            # n: number of items
                            # rho: penalty parameter
                            # sigmas = rho * (log(weights) - Xbeta - u)/weights is the additional term compared to ILSR
                            if (self$method_pi_tilde_init == 'OP'){ # HT: should this be QP? - currently unused
                                sigmas <- rho * (log(weights + epsilon) - x_beta - u)/(weights + epsilon)
                                weights <- self$init_ilsr_feat_convex_QP(weights, sigmas)
                            }
                            ilsr_conv <- FALSE
                            iter <- 0
                            while (!ilsr_conv){
                                sigmas <- rho * (log(weights + epsilon) - x_beta - u)/(weights + epsilon)
                                pi_sigmas <- weights * sigmas
                                #######################
                                # print('Log ADMM 0-mean', np.sum(pi_sigmas))
                                # indices of states for which sigmas < 0
                                ind_minus <- which(sigmas < 0)
                                # indices of states for which sigmas >= 0
                                ind_plus <- which(sigmas >= 0)
                                # sum of pi_sigmas over states for which sigmas >= 0
                                scaled_sigmas_plus <- sigmas[ind_plus] / sum(pi_sigmas[ind_minus])
                                # fill up the transition matrix
                                chain <- matrix(0, self$n, self$n)
                                # increase the outgoing rate from ind_plus to ind_minus
                                for (ind_minus_cur in ind_minus){
                                    chain[ind_plus, ind_minus_cur] <-
                                        pi_sigmas[ind_minus_cur] * scaled_sigmas_plus
                                }
                                for (r in seq_len(self$M)){
                                    ordering <- self$orderings[r,]
                                    sum_weights <- sum(weights[ordering]) + epsilon
                                    for (i in seq_len(self$n)){
                                        winner <- ordering[i]
                                        val <- 1L / sum_weights
                                        for (loser in ordering[-seq_len(i)]){
                                            chain[loser, winner] <- chain[loser, winner] + val
                                        }
                                        sum_weights <- sum_weights - weights[winner]
                                    }
                                }
                                # each row sums up to 0
                                chain <- chain - diag(rowSums(chain))
                                weights_prev <- weights
                                weights <- statdist(chain, method = "power",
                                                    v_init = weights,
                                                    n_iter = n_iter,
                                                    rtol = rtol)
                                # Check convergence
                                iter <- iter + 1
                                ilsr_conv <- sqrt(sum((weights_prev - weights)^2)) < rtol * sqrt(sum((weights)^2)) ||
                                    iter >= n_iter
                            }
                            # print('Log ADMM balance', check_global_balance_eqn(chain, weights))
                            weights
                        },
                        init_ilsr_feat_convex_QP = function(weights_prev, sigmas){
                            # sigmas is the additional term compared to ILSR
                            # min._{pi} ||pi-pi_{t-1}||^2, s.t. pi >=0 and sum(pi)=1 and sum(pi*sigma)=0
                            # :return initial weights for ilsr_feat which satisfy the mean-zero condition for MC

                            # Define variables
                            weights <- Variable(self$n)
                            # Define objective
                            objective <- Minimize(sum((weights - weights_prev)^2))
                            # Define constraints
                            constraints <- list(weights >= rtol,
                                               sum(weights) == 1,
                                               t(weights) %*% sigmas == 0)
                            # Optimize
                            prob <- Problem(objective, constraints = constraints)
                            res <- solve(prob, solver = "SCS") # splitting conic solver
                            res$getValue(weights)
                        }
                    ))
