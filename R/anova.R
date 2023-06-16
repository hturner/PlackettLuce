#' @importFrom stats pchisq
#' @method anova PLADMM
#' @export
anova.PLADMM <- function(object, ...) {
    dots <- list(...)
    if (length(dots)){
        object <- c(list(object), dots)
        all_pladmm <- all(vapply(object, inherits, logical(1), "PLADMM"))
        if (!all_pladmm)
            stop("all arguments to `anova.PLADMM` should PLADMM models")
        return(anova.PLADMMlist(object))
    }
    # null model
    nitem <- ncol(object$orderings)
    dev <- -2*objective(rep(1/nitem, nitem), object$orderings, object$weights)
    df <- object$df.residual + object$rank
    # sequential models
    term_list <- attr(terms(object), "term.labels")
    term_assign <- attr(object$x, "assign")
    nterms <- length(term_list)
    for (i in seq_len(nterms)) {
        fit <- pladmm_fit(orderings = object$orderings,
                          X = object$x[, term_assign <= i, drop = FALSE],
                          weights = object$weights,
                          start = object$coefficients[term_assign <= i],
                          rho = object$rho, maxit = object$maxit,
                          rtol = object$rtol)
        dev <- c(dev, -2*tail(fit$loglik, 1))
        df <- c(df, df[1] - sum(term_assign <= i) + 1)
    }
    # anova table
    res <- data.frame(Df = c(NA, -diff(df)),
                      Deviance = c(NA, -diff(dev)),
                      `Resid. Df` = df,
                      `Resid. Dev` = dev,
                      check.names = FALSE)
    res$`Pr(>Chi)` <- pchisq(abs(res$Deviance), abs(res$Df),
                             lower.tail = FALSE)
    # heading and structure
    structure(res,
              heading = c("Analysis of Deviance Table\n",
                          "Terms added sequentially (first to last)"),
              class = c("anova", "data.frame"))

}

anova.PLADMMlist <- function(object) {
    # check validity
    n <- vapply(object, function(x) nrow(x$orderings), numeric(1))
    if (any(n != n[1]))
        stop("the number of orderings is not the same for all models")
    # anova table
    dev <- vapply(object, deviance, numeric(1))
    df <- vapply(object, df.residual, numeric(1))
    res <- data.frame(`Resid. Df` = df,
                      `Resid. Dev` = dev,
                      Df = c(NA, diff(df)),
                      Deviance = c(NA, diff(dev)),
                      check.names = FALSE)
    res$`Pr(>Chi)` <- pchisq(abs(res$Deviance), abs(res$Df),
                             lower.tail = FALSE)
    # heading and structure
    models <- paste("Model ", seq_along(object), ": ",
                    lapply(object, function(x) formula(x$terms)),
                    sep = "", collapse = "\n")
    structure(res,
              heading = c("Analysis of Deviance Table\n",
                          models),
              class = c("anova", "data.frame"))
}
