# R port of ADMM_log class from admm_log.py as here:
# https://github.com/neu-spiral/FastAndAccurateRankingRegressionFunctions

#' @importFrom R6 R6Class
ADMM_log <- R6::R6Class("ADMM_log",
                        public = list(
                            n = NA,
                            p = NA,
                            M = NA,
                            X = NA,
                            X_ls = NA,
                            X_tilde = NA,
                            orderings = NA,
                            weights = NA,
                            method_pi_tilde_init = NA,
                            initialize = function(orderings, X, weights,
                                                  method_pi_tilde_init = "prev"){
                                # n: number of items
                                # p: number of features
                                # M: number of orderings
                                # :param orderings: (c_l, A_l): 1...M
                                # :param X: n*p, feature matrix
                                # :param method_pi_tilde_init: for ilsr_feat,
                                # initialize with prev_pi/orthogonal projection
                                self$n <- nrow(X)
                                self$p <- ncol(X)
                                self$M <- nrow(orderings)
                                self$X <- X
                                self$X_ls <- solve(crossprod(X), t(X))
                                self$X_tilde <- cbind("(Intercept)" = 1, X)
                                self$orderings <- orderings
                                self$weights <- weights
                                self$method_pi_tilde_init <-
                                    method_pi_tilde_init
                            },
                            fit_log = function(rho, pi, beta, u, gamma = 1,
                                               epsilon = .Machine$double.eps){
                                # :param rho: penalty parameter
                                # :param beta: parameter vector, px1
                                # :param pi: scores at each iteration, nx1
                                # :param u: scaled dual variable, nx1
                                # :param gamma: scaling on dual variable update
                                start <- Sys.time()
                                ## pi update
                                x_beta <- self$X_tilde %*% beta
                                pi <- self$ilsrx_log(rho = rho, pi = pi,
                                                     x_beta = x_beta, u = u,
                                                     weights = self$weights,
                                                     epsilon = epsilon)
                                ## dual update
                                u <- u + gamma * (x_beta - log(pi + epsilon))
                                ## beta_b update # uses svd
                                # beta = spl.lstsq(self.X, np.log(pi) - u)[0]
                                # HT: store in self later
                                X_ls <- solve(crossprod(self$X_tilde),
                                              t(self$X_tilde))
                                beta <- X_ls %*% (log(pi + epsilon) - u)
                                end <- Sys.time()
                                list(pi = drop(pi), beta = drop(beta),
                                     u = drop(u), time = (end - start))
                            },
                            ilsrx_log = function(rho, pi, x_beta, u, weights,
                                                 n_iter = 500,
                                                 rtol = 1e-4,
                                                 epsilon = .Machine$double.eps){
                                # modified spectral ranking algorithm for
                                # partial ranking data. Remove inner loop for
                                # top-1 ranking.
                                # n: number of items
                                # rho: penalty parameter
                                # sigmas = rho * (log(pi) - Xbeta - u)/pi
                                # is the additional term compared to ILSR
                                if (self$method_pi_tilde_init == 'OP'){
                                    # HT: should this be QP? - currently unused
                                    sigmas <-
                                        rho * (log(pi + epsilon) - x_beta - u)/
                                        (pi + epsilon)
                                    pi <-
                                        self$init_ilsr_feat_convex_QP(pi,
                                                                      sigmas)
                                }
                                ilsr_conv <- FALSE
                                iter <- 0
                                while (!ilsr_conv){
                                    sigmas <-
                                        rho * (log(pi + epsilon) - x_beta - u)/
                                        (pi + epsilon)
                                    pi_sigmas <- pi * sigmas
                                    #######################
                                    # print('Log ADMM 0-mean',
                                    #       np.sum(pi_sigmas))
                                    # indices of states for which sigmas < 0
                                    ind_minus <- which(sigmas < 0)
                                    # indices of states for which sigmas >= 0
                                    ind_plus <- which(sigmas >= 0)
                                    # sum of pi_sigmas over states for which
                                    # sigmas >= 0
                                    scaled_sigmas_plus <- sigmas[ind_plus] /
                                        sum(pi_sigmas[ind_minus])
                                    # fill up the transition matrix
                                    chain <- matrix(0, self$n, self$n)
                                    # increase the outgoing rate from
                                    # ind_plus to ind_minus
                                    for (ind_minus_cur in ind_minus){
                                        chain[ind_plus, ind_minus_cur] <-
                                            pi_sigmas[ind_minus_cur] *
                                            scaled_sigmas_plus
                                    }
                                    for (r in seq_len(self$M)){
                                        ordering <- self$orderings[r,]
                                        sum_pi <-
                                            sum(pi[ordering]) + epsilon
                                        for (i in seq_len(self$n)){
                                            winner <- ordering[i]
                                            val <- weights[r] / sum_pi
                                            for (loser in ordering[-seq_len(i)]){
                                                chain[loser, winner] <-
                                                    chain[loser, winner] + val
                                            }
                                            sum_pi <-
                                                sum_pi - pi[winner]
                                        }
                                    }
                                    # each row sums up to 0
                                    chain <- chain - diag(rowSums(chain))
                                    pi_prev <- pi
                                    pi <- statdist(chain, method = "power",
                                                   v_init = pi,
                                                   n_iter = n_iter,
                                                   rtol = rtol)
                                    # Check convergence
                                    iter <- iter + 1
                                    ilsr_conv <-
                                        sqrt(sum((pi_prev - pi)^2)) <
                                        rtol * sqrt(sum((pi)^2)) ||
                                        iter >= n_iter
                                }
                                # print('Log ADMM balance',
                                #       check_global_balance_eqn(chain, pi))
                                pi
                            },
                            init_ilsr_feat_convex_QP = function(pi_prev,
                                                                sigmas){
                                # sigmas is the additional term compared to ILSR
                                # min._{pi} ||pi-pi_{t-1}||^2, s.t. pi >=0 and
                                # sum(pi)=1 and sum(pi*sigma)=0
                                # :return initial pi for ilsr_feat which
                                # satisfy the mean-zero condition for MC

                                # Define variables
                                pi <- Variable(self$n)
                                # Define objective
                                objective <-
                                    Minimize(sum((pi - pi_prev)^2))
                                # Define constraints
                                constraints <- list(pi >= rtol,
                                                    sum(pi) == 1,
                                                    t(pi) %*% sigmas == 0)
                                # Optimize
                                prob <- Problem(objective,
                                                constraints = constraints)
                                # splitting conic solver
                                res <- solve(prob, solver = "SCS")
                                res$getValue(pi)
                            }
                        ))
