#' Preprocessor flowcam data
#'
#' Convert all \code{.tif} files in \code{flowcam} folder to zip compressed TIFF.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export

pre_processor_flowcam <- function(
  input,
  output
) {
  message("\n########################################################\n")
  message("\nProcessing flowcam...\n")
  ##

  if ( length( list.files( file.path(input, "flowcam") ) ) == 0 ) {
    message("\nEmpty or missing flowcam directory - nothing to do.\n")
    message("\ndone\n")
    message("########################################################\n")
    return(invisible(TRUE))
  }
  
  dir.create(
    file.path(output, "flowcam"),
    recursive = TRUE,
    showWarnings = FALSE
  )

 
  file.copy(
    file.path( input, "..", "00.general.parameter", "." ),
    file.path( output, "flowcam" ),
    recursive = TRUE,
    overwrite = TRUE
  )
  file.copy(
    from = file.path(input, "flowcam", "."),
    to = file.path(output, "flowcam"),
    recursive = TRUE
  )

  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
