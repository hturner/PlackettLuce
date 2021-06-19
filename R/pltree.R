#' Plackett-Luce Trees
#'
#' Recursive partitioning based on Plackett-Luce models.
#'
#' Plackett-Luce trees are an application of model-based recursive partitioning
#' (implemented in \code{\link[partykit]{mob}}) to Plackett-Luce models for
#' rankings. The partitioning is based on ranking covariates, e.g. attributes of
#' the judge making the ranking, or conditions under which the ranking is made.
#' The response should be a \code{\link{grouped_rankings}} object that groups
#' rankings with common covariate values. This may be included in a data frame
#' alongside the covariates.
#'
#' Most arguments of \code{PlackettLuce} can be passed on by \code{pltree}.
#' However, Plackett-Luce tree with fixed adherence are not implemented.
#' Arguably it makes more sense to estimate adherence or reliability within
#' the nodes of the Plackett-Luce tree.
#'
#' Various methods are provided for \code{"pltree"} objects, most of them
#' inherited from \code{"modelparty"} objects (e.g. \code{print},
#' \code{summary}), or \code{"bttree"} objects (\code{plot}). The plot
#' method employs the \code{\link[psychotree]{node_btplot}}
#' panel-generating function. The See Also
#' section gives details of separately documented methods.
#'
#' @param formula A symbolic description of the model to be fitted, of the form
#' \code{y ~ x1 + ... + xn} where \code{y} should be an object of class
#' \code{\link{grouped_rankings}} and \code{x1}, \dots, \code{xn} are used as
#'  partitioning variables.
#' @param data An optional data object containing the variables in the model.
#' Either a data frame of variables in `formula` or a list of length 2 giving
#' data frames for variables in `formula` and in `worth`.
#' @param worth A optional formula specifying a linear model for log-worth.
#' If `NULL`, worth is estimated separately for each item with
#' [`PlackettLuce()`]. Otherwise, the model in each node of the tree id fitted
#' with [`pladmm()`].
#' @param subset A specification of the rows to be used for variables in
#' `formula`, passed to \code{\link{model.frame}}.
#' @param na.action how NAs are treated for variables in `formula`, applied
#' to the underlying rankings and then passed to \code{\link{model.frame}}.
#' @param cluster an optional vector of cluster IDs to be employed for clustered
#' covariances in the parameter stability tests, see \code{\link{mob}}.
#' @param ref an integer or character string specifying the reference item (for
#' which log ability will be set to zero). If NULL the first item is used.
#' @param ... additional arguments, passed to \code{\link{PlackettLuce}}
#' of [`pladmm()`].
#' @return An object of class \code{"pltree"} inheriting from \code{"bttree"}
#' and \code{"modelparty"}.
#' @seealso
#'
#' \code{\link[psychotree]{bttree}} For fitting Bradley-Terry trees
#' (equivalent to the Plackett-Luce model for paired comparisons without ties).
#'
#' `coef`, `vcov`, `AIC` and `predict` methods are documented on
#' [`pltree-summaries`].
#'
#' [`itempar`][itempar.PlackettLuce], extracts the abilities or item parameters
#' in each node of the tree using \code{\link{itempar.PlackettLuce}}.
#'
#' [`fitted`][fitted.PlackettLuce], computes probabilities for the observed
#' choices based on the full tree.
#' @aliases pltree
#' @examples
#' # Bradley-Terry example
#'
#' if (require(psychotree)){
#'     ## Germany's Next Topmodel 2007 data
#'     data("Topmodel2007", package = "psychotree")
#'     ## convert paircomp object to grouped rankings
#'     R <- as.grouped_rankings(Topmodel2007$preference)
#'     ## rankings are grouped by judge
#'     print(R[1:2,], max = 4)
#'     ## Topmodel2007[, -1] gives covariate values for each judge
#'     print(Topmodel2007[1:2, -1])
#'
#'     ## fit partition model based on all variables except preference
#'     ## set npseudo = 0 as all judges rank all models
#'     tm_tree <- pltree(R ~ ., data = Topmodel2007[, -1], minsize = 5,
#'                       npseudo = 0)
#'
#'     ## plot shows abilities constrained to sum to 1
#'     plot(tm_tree, abbreviate = 1, yscale = c(0, 0.5))
#'     ## instead show log-abilities with Anja as reference (need to used index)
#'     plot(tm_tree, abbreviate = 1, worth = FALSE, ref = 6,
#'          yscale = c(-1.5, 2.2))
#'
#'     ## log-abilities, zero sum contrast
#'     itempar(tm_tree, log = TRUE)
#' }
#' @importFrom partykit mob_control
#' @export
pltree <- function (formula, data, worth, subset, na.action, cluster,
                    ref = NULL, ...){
    pltree_call <- match.call(expand.dots = TRUE)
    # handle model frame here to preserve attributes after na.action
    mf_args <- names(pltree_call) %in% c("formula", "data", "subset", "cluster")
    mf <- pltree_call[c(1L, which(mf_args))]
    mf$na.action <- "na.pass"
    if (!missing(worth)){
        # for now assume in order
        mf$data <- data[[1L]]
    }
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (missing(na.action)) na.action <- getOption("na.action")
    mf <- match.fun(na.action)(mf)
    # also need to handle NAs in partially NA grouped rankings
    if (attr(attr(mf, "terms"), "response")){
        mf[[1L]] <- na.omit(mf[[1L]])
    } else stop("`formula` must specify a response (grouped rankings)")
    # now set up for mob/fitting function
    control_args <- names(pltree_call) %in% names(formals(mob_control))
    control <- do.call("mob_control", as.list(pltree_call)[control_args])
    keep <-!names(pltree_call) %in% c("subset", "na.action", "cluster",
                            names(formals(mob_control)))
    mob_call <- pltree_call[keep]
    mob_call$data <- quote(mf)
    mob_call$control <- control
    if (!missing(worth)){
        # convert grouped rankings to grouped orderings
        ord <- convert_to_orderings(attr(mf[[1L]], "rankings"))
        attr(mf[[1L]], "rankings")[] <- ord[]

        # define model matrix for linear predictor
        spec <- model_spec(formula = worth, data = data[[2L]],
                           contrasts = pltree_call[["contrasts"]],
                           items = attr(ord, "items"))
        mob_call$worth <- spec$x
        mob_call$fit <- as.name("pladmm_mob_fit")
    } else{
        mob_call$fit <- as.name("plfit")
    }
    mob_call[[1L]] <- quote(partykit::mob)
    rval <- eval(mob_call, list(mf = mf), parent.frame())
    rval$info$call <- mob_call
    rval$info$call$data <- substitute(data)
    # replace data with mf (loses na.action attribute)
    rval$data <- mf
    # add in na.action
    rval$na.action <- attr(mf, "na.action")
    class(rval) <- c("pltree", "bttree", class(rval))
    return(rval)
}

#' @method plot pltree
#' @importFrom psychotree node_btplot
#' @export
plot.pltree <- function (x, terminal_panel = node_btplot,
                         tp_args = list(...),
                         tnex = NULL, drop_terminal = NULL, ...){
    NextMethod()
}

#' @method print pltree
#' @export
print.pltree <- function (x, title = "Plackett-Luce tree",
                          objfun = "negative log-likelihood",
                          ...) {
    partykit::print.modelparty(x, title = title, objfun = objfun,
                               ...)
}

#' @export
model.frame.pltree <- function(formula, ...){
    mf <- formula$data
    if (NROW(mf) > 0L)
        return(mf)
    mf <- formula$info$call
    # get args from call to start
    mf_args <- names(mf) %in% c("formula", "data", "subset", "na.action")
    mf <- mf[c(1L, which(mf_args))]
    # replace with those from current call if specified
    m <- match.call(expand.dots = TRUE)
    mf_args <- !(names(m) %in% "formula")
    mf[names(mf_args)] <- m[mf_args]
    # save and temporarily replace na.action
    na.action <- mf$na.action
    mf$na.action <- "na.pass"
    mf[[1L]] <- quote(stats::model.frame)
    if (is.null(env <- environment(formula$info$terms)))
        env <- parent.frame()
    mf <- eval(mf, env)
    if (is.null(na.action)) na.action <- getOption("na.action")
    mf <- match.fun(na.action)(mf)
}
