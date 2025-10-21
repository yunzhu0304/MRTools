#' Export MRTools Result to Cytoscape
#'
#' This function takes the output of \code{MRTools()} and sends it to Cytoscape
#' for visualization. It automatically creates a network using MR regulator
#' relationships, maps node color to log fold change (logFC), assigns shapes
#' based on TF status, and customizes node label fonts and sizes.
#'
#' @details
#' Before running this function, you must have Cytoscape installed and open
#' on your local computer. Wait until Cytoscape shows the message
#' "Automation API listening on port 1234" in its console, which indicates
#' that the REST API is available. Otherwise, the function will stop with
#' an error.
#'
#' @param MRTResult An object of class \code{"MRToolsResult"}, typically
#'   the output from \code{\link{MRTools}}. It must contain:
#'   \itemize{
#'     \item \code{MR_list}: a data.frame with \code{Source} and \code{Target} columns
#'     \item \code{MRgene_property}: a data.frame with gene properties including logFC and If_TF
#'   }
#' @param title Character. Title of the Cytoscape network. Default: \code{"MRTools Network"}.
#' @param collection Character. Name of the Cytoscape network collection.
#'   Default: \code{"MRTools"}.
#' @param shap_col Character. Column in \code{MRTResult$MRgene_property} used for
#'   node shape mapping. Default: \code{"If_TF"}.
#' @param NODE_LABEL_FONT_SIZE Numeric. Default font size of node labels.
#'   Default: \code{45}.
#' @param NODE_HEIGHT Numeric. Node height. Default: \code{60}.
#' @param NODE_WIDTH Numeric. Node width. Default: \code{120}.
#' @param NODE_LABEL_FONT_FACE Character. Node label font face.
#'   Default: \code{"Arial Rounded MT Bold"}.
#' @param ... Additional arguments (not used).
#'
#' @return None. The function creates and styles a network in an open Cytoscape
#'   session. Side effects include displaying messages about Cytoscape connectivity
#'   and applying visual mappings.
#'
#' @examples
#' \dontrun{
#' # Ensure Cytoscape is open locally before running:
#' MRToolsToCytoscape(MRTResult = MRTools_test_result)
#' }
#'
#' @import RCy3
#' @export
MRToolsToCytoscape <- function(MRTResult, title = "MRTools Network",collection = "MRTools",
                               shap_col = "If_TF",NODE_LABEL_FONT_SIZE = 45,
                               NODE_HEIGHT = 60, NODE_WIDTH = 120,
                               NODE_LABEL_FONT_FACE = "Arial Rounded MT Bold",
                               ...){

  cytoscape_is_up <- function(...) {
    ok <- tryCatch({
      info <- RCy3::cytoscapeVersionInfo()
      isTRUE(length(info) > 0)
    }, error = function(e) FALSE)

    if (ok) {
      cat("You are connected to Cytoscape!\n")
      invisible(TRUE)
    } else {
      cat("Cytoscape not available. Start Cytoscape and try again.")
      invisible(FALSE)
    }
  }


  if (!cytoscape_is_up()) {
    stop("Cytoscape not available. Start Cytoscape and try again.")
  }

  edges <- data.frame(
    source = MRTResult$MR_list$Source,
    target = MRTResult$MR_list$Target,
    interaction = "regulates"
  )

  nodes <- data.frame(id = unique(c(MRTResult$MR_list$Source, MRTResult$MR_list$Target)))
  nodes <- merge(nodes, MRTResult$MRgene_property,
                 by.x = "id", by.y = "GeneSymbol", all.x = TRUE)

  nodes$logFC <- ifelse(is.na(nodes$logFC),0,nodes$logFC)

  createNetworkFromDataFrames(nodes, edges,
                              title = title,
                              collection = collection)


  logfc_range <- range(nodes$logFC, na.rm = TRUE)

  setNodeColorMapping(
    table.column = "logFC",
    table.column.values = c(logfc_range[1], 0, logfc_range[2]),
    colors = c("#4393C3", "white", "#D6604D"),
    mapping.type = "c"
  )

  setNodeShapeMapping(
    table.column = shap_col,
    table.column.values = c("TF", "Others"),
    shapes = c("DIAMOND", "RECTANGLE")
  )

  setNodeLabelMapping(table.column = "id", style.name = "default")

  setVisualPropertyDefault(list(visualProperty = "NODE_LABEL_FONT_SIZE", value = NODE_LABEL_FONT_SIZE),
                           style.name = "default")



  setVisualPropertyDefault(
    list(visualProperty = "NODE_HEIGHT", value = NODE_HEIGHT),
    style.name = "default"
  )


  setVisualPropertyDefault(
    list(visualProperty = "NODE_WIDTH", value = NODE_WIDTH),
    style.name = "default"
  )

  setVisualPropertyDefault(
    list(visualProperty = "NODE_LABEL_FONT_FACE", value = NODE_LABEL_FONT_FACE),
    style.name = "default"
  )
}
