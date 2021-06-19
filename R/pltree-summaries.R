#' Plackett-Luce Tree Summaries
#'
#' Obtain the coefficients, variance-covariance matrix, AIC, or predictions
#' from a Plackett-Luce tree fitted by [pltree()].
#'
#' \code{AIC} computes \eqn{-2L + 2p}{-2 * L + 2 * p} where \eqn{L} is the
#' joint likelihood of the observed rankings under the tree model and
#' \eqn{p} is the degrees of freedom used to fit the tree model.
#'
#' @param object a fitted model object of class \code{"pltree"}.
#' @param node a vector of node ids specifying the nodes to summarise, by
#' default the ids of the terminal nodes.
#' @param drop if \code{TRUE} return the coefficients as a vector when only one
#' node is selected.
#' @param newdata an optional data frame to use instead of the
#' original data. For \code{AIC} this must include the response variable.
#' @param type the type of prediction to return for each group, one of:
#' \code{"itempar"} to give the result of \code{\link{itempar}} (by default the
#' fitted probability of each item being ranked first out of all objects),
#' \code{"rank"} the corresponding rank, \code{"best"} the topped ranked item,
#' or \code{"node"} the node of the tree the group belongs to.
#' @param ... additional arguments passed to
#' [`itempar`][itempar.PlackettLuce()] by \code{predict}, and to
#' \code{\link{model.frame}} by \code{AIC}.
#' @examples
#' data(beans)
#' # fit tree based on pairwise comparisons with variety B
#' pairB <- data.frame(Winner = ifelse(beans$var_b == "Worse",
#'                                     "Local", beans$variety_b),
#'                     Loser = ifelse(beans$var_b == "Worse",
#'                                    beans$variety_b, "Local"),
#'                     stringsAsFactors = FALSE, row.names = NULL)
#' beans$G <- as.rankings(pairB, input = "orderings",
#'                        index = rep(seq(nrow(beans)), 1))
#'
#' mod <- pltree(G ~ ., data = beans[c("G", "maxTN")])
#'
#' coef(mod, node = 3)
#' AIC(mod)
#'
#' # treat first row from each year as new data
#' newdata <- beans[!duplicated(beans$year),]
#'
#' ## fitted probabilities
#' predict(mod, newdata)
#'
#' ## fitted log-abilities, with Local as reference
#' predict(mod, newdata, log = TRUE, ref = "Local")
#'
#' ## variety ranks
#' predict(mod, newdata, type = "rank")
#'
#' ## top ranked variety
#' predict(mod, newdata, type = "best")
#'
#' ## node the trial belongs to
#' predict(mod, newdata, type = "node")
#' @name pltree-summaries
NULL

#' @rdname pltree-summaries
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
                          n <- length(info$coefficients) -
                              length(info$ties) + 1L
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

#' @rdname pltree-summaries
#' @method vcov pltree
#' @export
vcov.pltree <- function (object, node = nodeids(object, terminal = TRUE), ...){
    nodeapply(object, ids = node,
              FUN = function(n) vcov(info_node(n)$object, ...))
}

#' @rdname pltree-summaries
#' @method AIC pltree
#' @importFrom stats formula logLik model.frame model.response model.weights
#' @export
AIC.pltree <- function(object, newdata = NULL, ...) {
    if (is.null(newdata)) {
        return(NextMethod(object, ...))
    }
    # create model.frame from newdata
    response <- as.character(formula(object)[[2L]])
    if (!response %in% colnames(newdata))
        stop("`newdata` must include response")
    f <- formula(object)
    environment(f) <- parent.frame()
    newdata <- model.frame(f, data = newdata, ...)
    # predict node for each grouped rankingnodeids(tm_tree, terminal = TRUE)
    node <- partykit::predict.modelparty(object,
                                         newdata = newdata,
                                         type = "node")
    # set up to refit models based on newdata
    cf <- coef(object, log = FALSE)
    if (is.null(dim(cf))) cf <- t(as.matrix(cf))
    nodes <- partykit::nodeids(object, terminal = TRUE)
    dots <- object$info$dots
    G <- model.response(newdata)
    w <- model.weights(newdata)
    if (is.null(w)) w <- rep.int(1L, length(G))
    LL <- df <- numeric(length(nodes))
    for (i in seq_along(nodes)){
        # fit model with coef fixed to get logLik
        # suppress warning due to fixing maxit
        id <- node == nodes[i]
        if (sum(id)) {
            fit <- suppressWarnings(
                do.call("plfit",
                        c(list(y = G[id,],
                               start = cf[i,],
                               weights = w[id],
                               maxit = 0),
                          dots)))
            LL[i] <- -fit$objfun
        }
    }
    # compute AIC based on total log likelihood of data
    # and df of original model fit
    -2L*sum(LL) + 2L*attr(logLik(object), "df")
}

#' @rdname pltree-summaries
#' @method predict pltree
#' @export
predict.pltree <- function(object, newdata = NULL,
                           type = c("itempar", "rank", "best", "node"),
                           ...) {
    type <- match.arg(type)
    if (type == "node"){
        res <- partykit::predict.modelparty(object,
                                            newdata = newdata,
                                            type = "node")
        return(structure(as.character(res),
                         names = as.character(seq_along(res))))
    }
    if (is.null(newdata)) {
        newdata <- model.frame(object)
        na.action <- attr(newdata, "na.action")
    } else na.action <- NULL
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
    out <- partykit::predict.modelparty(object, newdata = newdata,
                                        type = pred, ...)
    if (is.null(na.action) | !inherits(na.action, "exclude")) return(out)
    n_miss <- length(na.action)
    current <- seq_len(NROW(out) + n_miss)[-na.action]
    id <- order(c(match(current, seq_len(NROW(out))), na.action))
    if (is.matrix(pred)){
        pred <- rbind(pred,
                      matrix(0L, nrow = n_miss, ncol = ncol(pred)))[id,]
    } else pred <- c(pred, rep.int(0L, n_miss))[id]
}

