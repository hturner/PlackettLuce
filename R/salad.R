#' Rankings of Salad Dressings
#'
#' Full rankings of four salad dressings collected by \cite{Vargo (1989)} and
#' published in \cite{Critchlow and Fligner (1991)}. The salad dressings are
#' formulated with varying percentages of acetic and gluconic acid as follows:
#' A = (0.5, 0), B = (0.5, 10.0), C = (1.0, 0), and D = (0, 10.0). The salad
#' dressings are ranked by tartness from 1 (most tart) to 4 (least tart).
#'
#' @format A data frame with 32 records across 4 variables: `A`, `B`, `C`, and
#' `D`, representing the four salad dressings.
#' @source
#' Critchlow, D. E. and Fligner, M. A. (1991) Paired comparison, triple
#' comparison, and ranking experiments as generalized linear models, and their
#' implementation on GLIM. _Psychometrika_ **56**(3), 517–-533.
#' @references
#' Vargo, M. D. (1989) Microbiological spoilage of a moderate acid food system
#' using a dairy-based salad dressing model. Unpublished masters thesis,
#' Ohio State University, Department of Food Science and Nutrition, Columbus,
#' OH, USA.
#' @examples
#'
#' # create data frame of acetic and gluconic acid concentrations
#' features <- data.frame(salad = LETTERS[1:4],
#'                        acetic = c(0.5, 0.5, 1, 0),
#'                        gluconic = c(0, 10, 0, 10))
#'
#' # fit Plackett-Luce model based on covariates
#' res_PLADMM <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)
#' # worth (relative tartness) as predicted by linear function of features
#' itempar(res_PLADMM)
"salad"
