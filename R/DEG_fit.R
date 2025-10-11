#' Format DEG List for MRTools Analysis
#'
#' This function takes a differential expression gene (DEG) list and reformats it
#' into a standardized structure for downstream analysis in the MRTools package.
#' Users can specify which columns contain gene symbols, log fold-change, and
#' adjusted p-values. The function also applies user-defined thresholds to classify
#' genes as \code{"up"}, \code{"down"}, or \code{"stable"}.
#'
#' @param data A \code{data.frame} containing differential expression results.
#' @param GeneSymbol A \code{character} string giving the column name for gene symbols.
#' @param logFC_from A \code{character} string giving the column name for log fold-change.
#' @param Pvalue_from A \code{character} string giving the column name for adjusted p-values.
#' @param logFC_THR A \code{numeric} value specifying the log fold-change threshold.
#'   Default is \code{1}.
#' @param P.Value_THR A \code{numeric} value specifying the adjusted p-value threshold.
#'   Default is \code{0.05}.
#' @param ... Additional arguments (currently not used).
#'
#' @return A \code{data.frame} with standardized columns:
#' \itemize{
#'   \item \code{GeneSymbol} Gene symbol.
#'   \item \code{logFC} Log fold-change.
#'   \item \code{pval} Adjusted p-value.
#'   \item \code{change} Differential expression status:
#'     \code{"up"}, \code{"down"}, or \code{"stable"}.
#' }
#'
#' @examples
#' # Example using demo dataset included in MRTools
#' data("DEG_demo")
#' DEG_list <- DEG_fit(
#'   data        = DEG_demo,
#'   GeneSymbol  = "GeneSymbol",
#'   logFC_from  = "logFC",
#'   Pvalue_from = "adj.P.Val"
#' )
#' head(DEG_list)
#'
#' @importFrom magrittr %>%
#' @export
DEG_fit <- function(data, GeneSymbol, logFC_from, Pvalue_from,
                    logFC_THR = 1, P.Value_THR = 0.05, ...) {

  data2 <- data %>%
    dplyr::select(
      GeneSymbol   = !!rlang::sym(GeneSymbol),
      logFC        = !!rlang::sym(logFC_from),
      pval         = !!rlang::sym(Pvalue_from)
    )

  S1 <- (data2$pval < P.Value_THR) & (data2$logFC < -logFC_THR)
  S2 <- (data2$pval < P.Value_THR) & (data2$logFC >  logFC_THR)

  data2 <- dplyr::mutate(
    data2,
    change = ifelse(S1, "down", ifelse(S2, "up", "stable"))
  )

  return(data2)
}
