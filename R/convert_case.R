#' Convert Gene Name Case by Species
#'
#' Standardize gene names from geneXplain outputs by species.
#' For human (\code{"Homo"}), convert to uppercase.
#' For mouse (\code{"Mm"}) and rat (\code{"Rn"}), capitalize the first letter
#' and lowercase the rest.
#'
#' @encoding UTF-8
#' @param data A data.frame or tibble; character/factor columns will be processed.
#' @param species One of \code{"Homo"}, \code{"Mm"}, or \code{"Rn"}.
#' @return A data.frame with gene names converted to the requested case.
#' @examples
#' \dontrun{
#' data("GeneXplain_source_target")
#' out <- convert_case(GeneXplain_source_target, species = "Rn")
#' }
#' @export
convert_case <- function(data, species = c("Homo", "Mm", "Rn")) {
  species <- match.arg(species)
  data[] <- lapply(data, function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (!is.character(x)) return(x)
    switch(
      species,
      "Homo" = toupper(x),
      "Mm"   = paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2))),
      "Rn"   = paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
    )
  })
  message(sprintf("✔ Gene names converted for species: %s", species))
  flush.console()
  return(data)
}
