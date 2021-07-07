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
  dir.create(
    file.path(output, "flowcam"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  loggit::set_logfile(file.path(output, "flowcam", "flowcam.log"))

  message("########################################################")
  message("Processing flowcam...")
  ##

  if ( length( list.files( file.path(input, "flowcam") ) ) == 0 ) {
    message("Empty or missing flowcam directory - nothing to do.")
    message("done")
    message("########################################################")
    return(invisible(TRUE))
  }



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

  message("done")
  message("########################################################")

  invisible(TRUE)
}
