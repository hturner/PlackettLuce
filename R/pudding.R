#' Paired Comparisons of Chocolate Pudding
#'
#' This is an example dataset from \cite{Davidson 1970} comprising paired
#' comparisons of chocolate pudding, with six brands in total. The responses
#' include tied outcomes, i.e. no preference.
#'
#' @format A data frame with 15 records and 6 variables:
#' \describe{
#'     \item{\code{i}}{The first brand in the comparison.}
#'     \item{\code{j}}{The second brand in the comparison.}
#'     \item{\code{r_ij}}{The frequency of paired comparisons of brand i and
#'     brand j.}
#'     \item{\code{w_ij}}{The frequency of preferences for i over j.}
#'     \item{\code{w_ji}}{The frequency of preferences for j over i.}
#'     \item{\code{t_ij}}{The frequency of no preference between i and j.}
#' }
#' @references
#' Davidson, R. R. (1970). On extending the Bradley-Terry model to accommodate
#' ties in paired comparison experiments. \emph{Journal of the American
#' Statistical Association}, \bold{65}, 317--328.
#' @examples
#'
#' # reshape to give one row per unique ranking
#' nr <- 3*nrow(pudding)
#' R <- matrix(0, nrow = nr, ncol = 6,
#'             dimnames = list(NULL, seq_len(6)))
#' i <- rep(pudding$i, 3)
#' j <- rep(pudding$j, 3)
#' R[cbind(seq_len(nr), i)] <- rep(c(1, 2, 1), each = nrow(pudding))
#' R[cbind(seq_len(nr), j)] <- rep(c(2, 1, 1), each = nrow(pudding))
#' # weights are frequencies of each ranking
#' w <- unlist(pudding[c("w_ij", "w_ji", "t_ij")])
#'
#' # fit Plackett-Luce model: limit iterations to match paper
#' mod <- PlackettLuce(R, npseudo = 0, weights = w, maxit = 7)
"pudding"
