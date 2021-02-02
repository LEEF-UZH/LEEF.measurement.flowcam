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
  
  ##
  processing <- file.path(normalizePath(output), "flowcam", paste0("PRE-PROCESSING.FLOWCAM", ".PROCESSING"))
  error <- file.path(normalizePath(output), "flowcam", paste0("ERROR.PRE-PROCESSING.PRE-PROCESSING", ".ERROR"))
  on.exit(
    {
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
      }
    }
  )
  file.create( processing )
  ##

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
  file.copy(
    from = file.path(input, "sample_metadata.yml"),
    to = file.path(output,  "flowcam","sample_metadata.yml")
  )

	unlink(processing)
  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
