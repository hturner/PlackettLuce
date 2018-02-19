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
#' Various methods are provided for \code{"pltree"} objects, most of them
#' inherited from \code{"modelparty"} objects (e.g. \code{print},
#' \code{summary}), or \code{"bttree"} objects (\code{plot}). \code{itempar}
#' extracts the abilities or item parameters from the Plackett-Luce models in
#' each node of the tree using \code{\link{itempar.PlackettLuce}}. The plot
#' method employs the \code{\link[psychotree]{node_btplot}}
#' panel-generating function.
#'
#' @param formula a symbolic description of the model to be fitted, of the form
#' \code{y ~ x1 + ... + xn} where \code{y} should be an object of class
#' \code{\link{grouped_rankings}} and \code{x1}, \dots, \code{xn} are used as
#'  partitioning variables.
#' @param data an optional data frame containing the variables in the model.
#' @param subset A specification of the rows to be used, passed to
#' \code{\link{model.frame}}.
#' @param na.action how NAs are treated, passed to \code{\link{model.frame}}.
#' @param cluster an optional vector of cluster IDs to be employed for clustered
#' covariances in the parameter stability tests, see \code{\link{mob}}.
#' @param ref an integer or character string specifying the reference item (for
#' which log ability will be set to zero). If NULL the first item is used.
#' @param ... additional arguments, passed to \code{\link{PlackettLuce}} by
#' \code{pltree} and to \code{\link{itempar}} by \code{predict}.
#' @param object a fitted model object of class \code{"pltree"}.
#' @param newdata an optional data frame to use for prediction instead of the
#' original data.
#' @param type the type of prediction to return for each group, one of:
#' \code{"itempar"} to give the result of \code{\link{itempar}} (by default the
#' fitted probability of each item being ranked first out of all objects),
#' \code{"rank"} the corresponding rank, \code{"best"} the topped ranked item,
#' or \code{"node"} the node of the tree the group belongs to.
#' @return An object of class \code{"pltree"} inheriting from \code{"bttree"}
#' and \code{"modelparty"}.
#' @seealso \code{\link[psychotree]{bttree}}
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
#'     print(R[1:2,], max = 6)
#'     ## Topmodel2007[, -1] gives covariate values for each judge
#'     head(Topmodel2007[, -1], 2)
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
#'     ## abilities with Anja as reference
#'     itempar(tm_tree, ref = "Anja")
#'
#'     ## results for the first three judges
#'     newdata <- Topmodel2007[1:3,]
#'     ### fitted probabilities
#'     predict(tm_tree, newdata)
#'     ### fitted log-abilities, with Anni as reference
#'     predict(tm_tree, newdata, log = TRUE, ref = "Anni")
#'     ###  item ranks
#'     predict(tm_tree, newdata, type = "rank")
#'     ### top ranked item
#'     predict(tm_tree, newdata, type = "best")
#'     ### node the judge belongs to
#'     predict(tm_tree, newdata, type = "node")
#'
#'}
#' @importFrom partykit mob_control
#' @export
pltree <- function (formula, data, subset, na.action, cluster, ref = NULL, ...){
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

#' @importFrom partykit mob
#' @export
partykit::mob

#' @method print pltree
#' @export
print.pltree <- function (x, title = "Plackett-Luce tree",
                          objfun = "negative log-likelihood",
                          ...) {
    partykit::print.modelparty(x, title = title, objfun = objfun,
                               ...)
}

#' @method plot pltree
#' @export
plot.pltree <- function (x, terminal_panel = psychotree::node_btplot,
                         tp_args = list(...),
                         tnex = NULL, drop_terminal = NULL, ...){
    requireNamespace("psychotree")
    NextMethod()
}

#' @method coef pltree
#' @importFrom partykit info_node nodeids
#' @export
coef.pltree <- function (object, node = NULL, drop = TRUE, ...) {
    if (is.null(node)){
        ids <- nodeids(object, terminal = TRUE)
    } else ids <- node
    if ("object" %in% object$info$control$terminal) {
        cf <- do.call("rbind",
                      lapply(ids, FUN = function(n, ...){
                          # set ref as specified in plfit if unspecified
                          info <- info_node(object[[n]]$node)
                          cll <- as.call(list(coef.PlackettLuce,
                                              info$object, ...))
                          cll <- match.call(coef.PlackettLuce, cll)
                          if (!"ref" %in% names(cll)) {
                              cll$ref <- attr(info$coefficients, "ref")
                          }
                          eval(cll)
                      }, ...))
    } else{
        cf <- do.call("rbind",
                      lapply(ids, FUN = function(n, ...){
                          # compute coef as returned from original fit
                          info <- info_node(object[[n]]$node)
                          n <- length(info$coefficients) - info$maxTied + 1
                          info$coefficients <- exp(info$coefficients)
                          id <- seq_len(n)
                          info$coefficients[id] <-
                              info$coefficients[id]/sum(info$coefficients[id])
                          # parameterize as requested
                          cll <- as.call(list(coef.PlackettLuce, info, ...))
                          cll <- match.call(coef.PlackettLuce, cll)
                          if (!"ref" %in% names(cll)) {
                              cll$ref <- attr(info$coefficients, "ref")
                          }
                          eval(cll)
                      }, ...))
    }
    rownames(cf) <- ids
    if (drop) {
        drop(cf)
    } else {
        cf
    }
}

#' @rdname pltree
#' @method predict pltree
#' @importFrom stats model.frame model.response model.weights
#' @export
predict.pltree <- function(object, newdata = NULL,
                           type = c("itempar", "rank", "best", "node", "aic"),
                           ...) {
        type <- match.arg(type)
        if (type %in% c("aic", "node")){
            res <- partykit::predict.modelparty(object,
                                                newdata = newdata,
                                                type = "node")
            if (type == "node"){
                return(structure(as.character(res),
                                 names = as.character(seq_along(res))))
            }
        }
        if (is.null(newdata))
            newdata <- model.frame(object)
        if (type == "aic"){
            cf <- itempar(object)
            nodes <- rownames(cf)
            dots <- object$info$dots
            G <- model.response(newdata) # requires model.frame
            w <- model.weights(newdata)
            if (is.null(w)) w <- rep.int(1, length(G))
            LL <- df <- numeric(length(nodes))
            for (i in seq_along(nodes)){
                # fit model with coef fixed to get logLik
                # suppress warning due to fixing maxit
                fit <- suppressWarnings(
                    do.call("plfit",
                            c(list(y = G[res == nodes[i],],
                                   start = cf[i,],
                                   weights = w[res == nodes[i]],
                                   maxit = 0),
                                   dots)))
                LL[i] <- -fit$objfun
            }
            # compute AIC based on total log likelihood of data
            # and df of original model fit
            return(-2*sum(LL) + 2*attr(logLik.PlackettLuce(object), "df"))
        }
        pred <- switch(type,
                       itempar = function(obj, ...) {
                           t(as.matrix(itempar(obj, ...)))
                       },
                       rank = function(obj, ...) {
                           t(as.matrix(rank(-obj$coefficients)))
                       },
                       best = function(obj, ...) {
                           nm <- names(obj$coefficients)
                           nm[which.max(obj$coefficients)]
                       })
        partykit::predict.modelparty(object, newdata = newdata, type = pred,
                                     ...)
    }
