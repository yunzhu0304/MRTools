#' Get Transcription Factor List by Species
#'
#' This function retrieves a transcription factor (TF) list for a given species
#' from the \code{TFLink} dataset included in the MRTools package.
#' The \code{TFLink.rda} file must be stored in the \code{data/} folder of the package
#' and contain three data frames: \code{TF_Homo}, \code{TF_Mm}, and \code{TF_Rn}.
#'
#' @param species A \code{character} string specifying the species.
#'   Must be one of \code{"Homo"} (human), \code{"Mm"} (mouse), or \code{"Rn"} (rat).
#'
#' @return A \code{data.frame} containing the transcription factor list for the chosen species.
#'
#' @examples
#' \dontrun{
#' # Load human TF list
#' tf_human <- get_TFlist("Homo")
#' head(tf_human)
#'
#' # Load mouse TF list
#' tf_mouse <- get_TFlist("Mm")
#'
#' # Load rat TF list
#' tf_rat <- get_TFlist("Rn")
#' }
#'
#' @export
get_TFlist <- function(species = c("Homo", "Mm", "Rn")) {
  species  <- match.arg(species)
  obj_name <- paste0("TF_", species)

  if (!exists(obj_name, envir = parent.env(environment()), inherits = FALSE)) {
    stop(sprintf("Internal TF data '%s' not found. Ensure R/sysdata.rda contains it.", obj_name))
  }

  TFlist <- get(obj_name, envir = parent.env(environment()), inherits = FALSE)
  message(sprintf("✔ Loaded TF list for species: %s", species))
  return(TFlist)
}
