#' Check if Cytoscape is running (internal)
#'
#' This internal helper function tests whether Cytoscape is available
#' via the REST API. It prints a message:
#' \itemize{
#'   \item "You are connected to Cytoscape!" if Cytoscape is up.
#'   \item "Cytoscape not available. Start Cytoscape and try again." if not.
#' }
#'
#' @return Invisibly returns \code{TRUE} if Cytoscape is running, otherwise \code{FALSE}.
#' @details
#' Uses \code{RCy3::cytoscapeVersionInfo()} inside a \code{tryCatch}.
#' Intended for internal use only.
#'
#' @keywords internal
cytoscape_is_up <- function(...) {
  ok <- tryCatch({
    info <- RCy3::cytoscapeVersionInfo()  # 成功会返回一个字符向量
    isTRUE(length(info) > 0)
  }, error = function(e) FALSE)

  if (ok) {
    cat("You are connected to Cytoscape!\n")  # 和 cytoscapePing 的提示保持一致
    invisible(TRUE)
  } else {
    cat("Cytoscape not available. Start Cytoscape and try again.")
    invisible(FALSE)
  }
}
