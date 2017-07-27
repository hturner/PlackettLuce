# 1. create a fit function as required by psychotools::mob
plfit <- function (y, x = NULL, start = NULL, weights = NULL, offset = NULL,
                   cluster = NULL, ..., estfun = FALSE, object = FALSE) {
    x <- !(is.null(x) || NCOL(x) == 0L)
    weights <- !is.null(weights)
    offset <- !is.null(offset)
    if (x || weights || offset)
        warning("unused argument(s): ",
                paste(c("x"[x], "weights"[weights], "offset"[offset]),
                      collapse = ","))
    # y is grouped_rankings object
    R <- attr(y, "rankings")
    # return null result if network not strongly connected
    if (attr(R, "no") > 1){
        return(list(coefficients = NA, objfun = Inf,
                    estfun = NULL, object = NULL))
    }
    res <- PlackettLuce(R, ...)
    # returns with rownames - possible to avoid?
    if (estfun) {
        percomp <- estfun.PlackettLuce(res)
        estfun <- rowsum(as.matrix(percomp), attr(y, "index"))
    } else estfun <- NULL
    list(coefficients = coef(res), objfun = -res$loglik,
         estfun = estfun,
         object = if (object) res else NULL)
}

# 2. create a pltree function
pltree <- function (formula, data, subset, na.action, cluster, ref = NULL, ...)
{
    m <- match.call(expand.dots = TRUE)
    control_args <- names(m) %in% names(formals(mob_control))
    control <- do.call("mob_control", as.list(m)[control_args])
    m <- m[!control_args]
    m$control <- control
    m$fit <- as.name("plfit")
    m[[1L]] <- as.name("mob")
    rval <- eval(m, parent.frame())
    rval$info$call <- m
    class(rval) <- c("pltree", "bttree", class(rval))
    return(rval)
}
