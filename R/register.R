#' Register the processing of flowcam data in the LEEF.Data package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom LEEF.Data add_pre_processor add_extractor
#' @export
#'
register <- function() {
  LEEF.Data::add_pre_processor( pre_processor_flowcam )
  LEEF.Data::add_extractor( extractor_flowcam )

  invisible(TRUE)
}

