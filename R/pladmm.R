# wrapper function to fit Plackett-Luce model with PLADMM-log, based on Python
# code in run_methods.py as here:
# https://github.com/neu-spiral/FastAndAccurateRankingRegressionFunctions
#' Fit a Plackett-Luce Model with Linear Predictor for Log-worth
#'
#' Fit a Plackett-Luce model where the log-worth is predicted by a linear
#' function of covariates. The rankings may be partial
#' (each ranking completely ranks a subset of the items), but ties are not
#' supported.
#'
#' The log-worth is modelled as a linear function of item covariates:
#' \deqn{\log \alpha_i = \beta_0 + \beta_1 x_{i1} + \ldots + \beta_p x_{ip}}{
#' log \alpha_i = \beta_0 + \beta_1 x_{i1} + ... + \beta_p x_{ip}
#' }
#' where \eqn{\beta_0} is fixed by the constraint that
#' \eqn{\sum_i \alpha_i = 1}.
#'
#' The parameters are estimated using an Alternating Directions Method of
#' Multipliers (ADMM) algorithm proposed by Yildiz (2020). ADMM alternates
#' between estimating the worths \eqn{\alpha_i} and the linear
#' coefficients \eqn{\beta_k}, encapsulating them in a quadratic penalty on the
#' likelihood:
#' \deqn{L(\boldsymbol{\beta}, \boldsymbol{\alpha}, \boldsymbol{u}) =
#' \mathcal{L}(\mathcal{D}|\boldsymbol{\alpha}) +
#' \frac{\rho}{2}||\boldsymbol{X}\boldsymbol{\beta} -
#' \log \boldsymbol{\alpha} + \boldsymbol{u}||^2_2 -
#' \frac{\rho}{2}||\boldsymbol{u}||^2_2}{
#' L(\beta, \alpha, u) = L(D|\alpha) + \rho/2 ||X\beta - log \alpha + u||^2_2 -
#' \rho/2 ||u||^2_2
#' }
#' where \eqn{\boldsymbol{u}}{u} is a dual variable that imposes the equality
#' constraints (so that \eqn{\log \boldsymbol{\alpha}}{log \alpha} converges to
#' \eqn{\boldsymbol{X}\boldsymbol{\beta}}{X \beta}).
#'
#' @param rankings a \code{"\link{rankings}"} object, or an object that can be
#' coerced by \code{as.rankings}.
#' @param formula a [formula] specifying the linear model for log-worth.
#' @param data a data frame containing the variables in the model.
#' @param start starting values for the coefficients.
#' @param contrasts an optional list specifying contrasts for the factors in
#' `formula`. See the `contrasts.arg` of [model.matrix()].
#' @param rho the penalty parameter in the penalized likelihood, see details.
#' @param n_iter the maximum number of iterations (also for inner loops).
#' @param rtol the convergence tolerance (also for inner loops)
#'
#' @note This is a prototype function and the user interface is planned to
#' change in upcoming versions of PlackettLuce.
#'
#' @references
#' Yildiz, I., Dy, J., Erdogmus, D., Kalpathy-Cramer, J., Ostmo, S.,
#' Campbell, J. P., Chiang, M. F. and Ioannidis, S. (2020) Fast and Accurate
#' Ranking Regression In Proceedings of the Twenty Third International
#' Conference on Artificial Intelligence and Statistics, \bold{108}, 77–-88.
#'
#' @examples
#'
#' if (require(prefmod)){
#'   data(salad)
#'   # data.frame of rankings for salad dressings A B C D
#'   # 1 = most tart, 4 = least tart
#'   salad[1:3,]
#'
#'   # create data frame of corresponding features
#'   # (acetic and gluconic acid concentrations in salad dressings)
#'   features <- data.frame(salad = LETTERS[1:4],
#'                          acetic = c(0.5, 0.5, 1, 0),
#'                          gluconic = c(0, 10, 0, 10))
#'
#'   # fit Plackett-Luce model based on covariates
#'   res_PLADMM <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)
#'   ## coefficients
#'   coef(res_PLADMM)
#'   ## worth
#'   res_PLADMM$pi
#'   ## worth as predicted by linear function
#'   res_PLADMM$tilde_pi
#'   ## equivalent to
#'   drop(exp(res_PLADMM$x %*% coef(res_PLADMM)))
#'
#' }
#'
#' @importFrom stats model.matrix
#' @export
pladmm <- function(rankings, # rankings object as used in PlackettLuce
                   formula, # formula for linear predictor
                   data = NULL,
                   start = NULL, # starting values for the beta coefficients
                   contrasts = NULL,
                   rho = 1, # penalty parameter
                   n_iter = 500, # main iter, pi update & stationary dist
                   # used in convergence checks: main iter, init of beta (
                   # & pi if QP init used), pi update & stationary dist
                   rtol = 1e-4
                   ){
    call <- match.call()

    # convert dense rankings to orderings as used in original Python functions
    orderings <- convert_to_orderings(rankings)

    # model spec (model matrix and terms)
    spec <- model_spec(formula = formula, data = data,
                       contrasts = contrasts, items = colnames(rankings))

    # model fit
    fit <- pladmm_fit(orderings = orderings, X = spec$x,
                      start = start,
                      rho = rho, maxit = n_iter,
                      rtol = rtol)

    # rank
    # (would complain earlier if X not full rank; -1 due to constraint on pi)
    rank <- ncol(spec$x) - 1
    # df.residual (for now assume all rankings same length)
    n <- ncol(orderings)
    M <- nrow(orderings)
    # frequencies of sets selected from, for sizes 2 to max observed
    freq <- M*(n - 1)
    # number of possible selections overall (can only choose 1 from each set)
    n_opt <-  M*(n*(n + 1)/2 - 1)
    df.residual <- n_opt - sum(freq) - rank

    fit <- c(# results from pladmm_fit
        fit,
        # supplementary
        list(call = call,
             x = spec$x,
             terms = spec$terms,
             xlevels = spec$xlevels,
             contrasts = spec$contrasts,
             orderings = orderings,
             rank = rank,
             df.residual = df.residual))
    class(fit) <- "PLADMM"
    fit
}

convert_to_orderings <- function(rankings){
    # (allow for partial rankings)
    rankings <- as.matrix(as.rankings(rankings))
    rankings[rankings == 0] <- NA
    part_order <- function(x) c(order(x, na.last = NA), numeric(sum(is.na(x))))
    structure(t(apply(rankings, 1, part_order)),
              items = colnames(rankings))
}

#' @importFrom stats .getXlevels terms
model_spec <- function(formula, data,
                       contrasts, items){
    # create model frame: can use data from environment of formula
    model_data <- model.frame(formula, data = data,
                              drop.unused.levels = TRUE)
    # check variables correct length
    if (nrow(model_data) != length(items))
        stop("length of variables in `formula` ",
             "does not match the number of items")

    # create model terms and model matrix
    model_terms <- terms(formula, data = data)
    X <- model.matrix(model_terms, data = model_data,
                      contrasts.arg = contrasts)
    rownames(X) <- items
    # check model contains an intercept
    # (don't use terms as intercept may be included in matrix term)
    if (!all(X[,1] == 1L))
        stop("`formula` must contain an intercept")

    list(x = X, terms = model_terms,
         xlevels = .getXlevels(model_terms, model_data),
         contrasts = contrasts)
}

pladmm_fit <- function(orderings, # low-level fit uses orderings
                       X, # model.matrix for linear predictor of worth
                       start = NULL, # starting values for the beta coefficients
                       rho = 1, # penalty parameter
                       maxit = 500, # main iter, pi update & stationary dist
                       # used in convergence checks: main iter, init of beta (
                       # & pi if QP init used), pi update & stationary dist
                       rtol = 1e-4){
    epsilon <- .Machine$double.eps # added to pi avoid logging zero

    n <- dim(X)[1]
    pi_init <- rep.int(1/n, n)

    # initialization of parameters
    pi_iter <- rep.int(1/n, n)
    u_iter <- numeric(n)
    if (is.null(start)) {
        ## pairwise probability of win/loss
        mat_Pij <- est_Pij(n, orderings)
        init <- init_exp_beta(X[,-1, drop = FALSE], orderings, mat_Pij)
        ## beta coef
        beta_iter <- c("(Intercept)" = 0, init$exp_beta_init)
        ## set intercept so that exp(X*beta_iter) sum to 1
        lambda <- X %*% beta_iter
        beta_iter[1] <- -log(sum(exp(lambda)))
    } else beta_iter <- start
    tilde_pi_iter <- drop(exp(X %*% beta_iter))

    # set statistics for optimisation/monitoring
    diff_pi <- norm(pi_iter)
    diff_beta <- norm(beta_iter)
    prim_feas <- norm(X %*% beta_iter - log(pi_iter + epsilon))
    dual_feas <- norm(t(X) %*% log(pi_iter + epsilon))
    obj <- objective(tilde_pi_iter, orderings)

    # iterative updates
    iter <- 0
    time <- numeric(0)
    log_admm <- ADMM_log$new(orderings, X[,-1, drop = FALSE],
                             method_pi_tilde_init = "prev")
    conv <- FALSE
    for (iter in seq_len(maxit)){
        # log_admm update
        if (!conv){
            pi_prev <- pi_iter
            beta_prev <- beta_iter
            tilde_pi_prev <- tilde_pi_iter

            res <- log_admm$fit_log(rho = rho, weights = pi_iter,
                                    beta = beta_iter, u = u_iter)
            pi_iter <- res$weights
            beta_iter <- res$beta
            b_iter <- res$b
            u_iter <- res$u
            time_iter <- res$time
            # scores predicted by beta
            tilde_pi_iter <- drop(exp(X %*% beta_iter))
            time <- c(time, time_iter)
            diff_pi <- c(diff_pi, norm(pi_prev - pi_iter))
            diff_beta <- c(diff_beta, norm(beta_prev - beta_iter))
            prim_feas <- c(prim_feas,
                           norm(X %*% beta_iter - log(pi_iter + epsilon)))
            dual_feas <- c(dual_feas,
                           norm(t(X) %*% (log(pi_prev + epsilon) -
                                              log(pi_iter + epsilon))))
            obj <- c(obj, objective(tilde_pi_iter, orderings, epsilon))
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

    # name outputs related to items
    names(pi_iter) <- names(u_iter) <- names(tilde_pi_iter) <-
        attr(orderings, "items")

    # return result of fit
    list(# parameters from last iteration
        coefficients = beta_iter,
        pi = pi_iter,
        u = u_iter, tilde_pi = tilde_pi_iter,
        # stored information from all iterations
        time = time_cont, diff_pi = diff_pi, diff_beta = diff_beta,
        prim_feas = prim_feas, dual_feas = dual_feas,
        loglik = obj,
        # algorithm status
        iter = iter, conv = conv)
}
