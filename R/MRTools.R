#' Construct and Prune Master Regulator Network
#'
#' This function integrates differential expression data (DEG) with
#' master regulator networks exported from \code{geneXplain}.
#' It performs standardized gene name conversion, network pruning
#' based on DEG status, and annotation of transcription factors (TFs).
#'
#' @param DEG_data A \code{data.frame} created by \code{\link{DEG_fit}},
#'   containing standardized differential expression results
#'   (\code{GeneSymbol}, \code{logFC}, \code{pval}, \code{change}).
#' @param GeneXplain_file A \code{data.frame} containing a source-target
#'   interaction network downloaded from \code{geneXplain}.
#'   Must include two columns specifying source and target genes.
#' @param GXP_source A \code{character} string giving the column name of
#'   the source gene in \code{GeneXplain_file}.
#' @param GXP_target A \code{character} string giving the column name of
#'   the target gene in \code{GeneXplain_file}.
#' @param species A \code{character} string specifying the species for
#'   case conversion and TF annotation. Must be one of \code{"Homo"},
#'   \code{"Mm"}, or \code{"Rn"}.
#' @param ... Additional arguments (currently not used).
#'
#' @return An object of class \code{MRToolsResult}, which is a list with:
#' \itemize{
#'   \item \code{MR_list} The pruned master regulator network
#'         (data.frame with Source and Target).
#'   \item \code{MRgene_property} A data.frame of regulators present in
#'         the pruned network, annotated with differential expression
#'         properties and TF status.
#' }
#'
#' @details
#' The pruning procedure iteratively removes targets that are not
#' differentially expressed or are stable, unless they serve as TFs
#' within the network. The process repeats until the network reaches
#' a stable structure (no further changes in row count).
#'
#' @examples
#' \dontrun{
#' # Load demo data
#' data("DEG_demo")
#' data("GeneXplain_source_target")
#'
#' # Format DEG list
#' DEG_list <- DEG_fit(
#'   data        = DEG_demo,
#'   GeneSymbol  = "GeneSymbol",
#'   logFC_from  = "logFC",
#'   Pvalue_from = "adj.P.Val"
#' )
#'
#' # Run MRTools analysis
#' MRTools_test_result <- MRTools(
#'   DEG_data       = DEG_list,
#'   GeneXplain_file= GeneXplain_source_target,
#'   GXP_source     = "Source",
#'   GXP_target     = "Target",
#'   species        = "Rn"
#' )
#'
#' # Inspect results
#' head(MRTools_test_result$MR_list)
#' head(MRTools_test_result$MRgene_property)
#'
#' # Save results to CSV
#'
#' openxlsx::write.xlsx(MRTools_test_result$MR_list,
#' file = "MRTools_network.xlsx")
#'
#' openxlsx::write.xlsx(MRTools_test_result$MRgene_property,
#' file = "MRTools_gene_property.xlsx")
#'
#'
#' }
#'
#' @export
MRTools <- function(DEG_data, GeneXplain_file, GXP_source, GXP_target,
                    species = c("Homo", "Mm", "Rn"), ...) {

  GeneXplain_file2 <- GeneXplain_file %>%
    dplyr::select(
      Source   = !!rlang::sym(GXP_source),
      Target   = !!rlang::sym(GXP_target))

  GeneXplain_file2 <- convert_case(GeneXplain_file2, species)


  MRgene_change <- c(unique(c(as.character(GeneXplain_file2[,1]),
                              as.character(GeneXplain_file2[,2]))))

  MRgene_change <- DEG_data[DEG_data$GeneSymbol %in% MRgene_change,]

  df <- GeneXplain_file2
  MR_list1 <- GeneXplain_file2

  for (j in 1:1000) {

    prev_nrow <- nrow(df)

    for (i in 1:length(unique(MR_list1$Target))) {

      gene <- unique(MR_list1$Target)[i]

      if (gene %in% MRgene_change$GeneSymbol) {
        if (sum(df$Source %in% gene) < 1 & MRgene_change$change[MRgene_change$GeneSymbol == gene] == "stable") {
          df2 <- df[df$Target != gene, ]
        } else {
          df2 <- df
        }
      } else {
        if (sum(df$Source %in% gene) < 1) {
          df2 <- df[df$Target != gene, ]
        } else {
          df2 <- df
        }
      }

      df <- df2
    }

    if (nrow(df) == prev_nrow) {
      message("✔ Network pruning finished")
      break
    }else{
      MR_list1 <- df
    }
  }

  MRpruned_FC <- c(unique(c(as.character(df[,1]),as.character(df[,2]))))
  MRpruned_FC <- DEG_data[DEG_data$GeneSymbol %in% MRpruned_FC,]

  TFlist <- get_TFlist(species)
  TFlist$TF <- TFlist$Name.TF
  tflist <- as.character(TFlist$TF)
  MRpruned_FC$If_TF <- ifelse(MRpruned_FC$GeneSymbol %in% tflist, "TF", "Others")

  MRTools_result <- structure(list(
    MR_list         = as.data.frame(df),
    MRgene_property = as.data.frame(MRpruned_FC)
  ), class = "MRToolsResult")

  return(MRTools_result)
}
