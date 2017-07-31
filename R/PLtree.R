#' Plackett-Luce Trees
#'
#' Recursive partitioning based on Plackett-Luce models.
#'
#' Plackett-Luce trees are an application of model-based recursive partitioning
#' (implemented in \code{\link[partykit]{mob}}) to Plackett-Luce models for
#' rankings. The partitioning is based on attributes of the subjects ranking the
#' items. Thus the response should be a
#' \code{\link{grouped_rankings}} object that groups rankings by subject. This
#' may be included in a data frame alongside the subject covariates.
#'
#' The returned object inherits from \code{"\link[psychotree]{bttree}"}, therefore the
#' psychotree package is required so that the methods are available.
#'
#' Various methods are provided for \code{"pltree"} objects, most of them
#' inherited from \code{"modelparty"} objects (e.g. \code{print}, \code{summary}), or
#' \code{"bttree"} objects (\code{itempar}, \code{plot}). \code{itempar}
#' extracts the abilities or item parameters from the Plackett-Luce models in
#' each node of the tree using \code{\link{itempar.PlackettLuce}}. The plot
#' method employs the \code{\link[psychotree]{node_btplot}}
#' panel-generating function.
#'
#' @param formula a symbolic description of the model to be fitted, of the form
#' \code{y ~ x1 + ... + xn} where \code{y} should be an object of class \code{\link{grouped_rankings}} and \code{x1}, \dots, \code{xn} are used as partitioning variables.
#' @param data an optional data frame containing the variables in the model.
#' @param subset A specification of the rows to be used, passed to
#' \code{\link{model.frame}}.
#' @param na.action how NAs are treated, passed to \code{\link{model.frame}}.
#' @param cluster an optional vector of cluster IDs to be employed for clustered
#' covariances in the parameter stability tests, see \code{\link{mob}}.
#' @param ref an integer or character string specifying the reference item (for which log ability will be set to zero). If NULL the first item is used.
#' @param ... additional arguments passed to \code{\link{PlackettLuce}}
#' @return An object of class \code{"pltree"} inheriting from \code{"bttree"}
#' and \code{"modelparty"}.
#' @seealso \code{\link[psychotree]{bttree}}
#' @examples
#' # Bradley-Terry example
#'
#' if (require(psychotree)){
#'     ## Germany's Next Topmodel 2007 data
#'     data("Topmodel2007", package = "psychotree")
#'     ## convert paircomp object to grouped rankings
#'     R <- as.grouped_rankings(Topmodel2007$preference)
#'
#'     ## fit partition model based on all variables except preference
#'     ## currently gives warnings because PlackettLuce does not accept weights
#'     ## but all weights are 1 here
#'     tm_tree <- pltree(R ~ ., data = Topmodel2007[, -1], minsize = 5)
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
#'}
#' @importFrom partykit mob_control
#' @export
pltree <- function (formula, data, subset, na.action, cluster, ref = NULL, ...){
    requireNamespace("psychotree")
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

#' @method predict pltree
#' @export
predict.pltree <- function(object, ...){
    # do not pass on to predict.bttree
    stop('Method not yet implemented for "pltree" objects.')
}
