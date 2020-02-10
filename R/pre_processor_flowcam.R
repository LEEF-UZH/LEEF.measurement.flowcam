#' Preprocessor flowcam data
#'
#' Convert all \code{.tif} files in \code{flowcam} folder to zip compressed TIFF.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom R.utils bzip2
#' @importFrom tiff readTIFF writeTIFF
#' @importFrom parallel mclapply detectCores
#' @export

pre_processor_flowcam <- function(
  input,
  output
) {
  message("\n########################################################\n")
  message("\nProcessing flowcam...\n")
  ##
  tmpdir <- tempfile()
  dir.create( tmpdir )
  ##
  file.copy(
    from = file.path(input, "flowcam", "."),
    to = tmpdir,
    recursive = TRUE
  )
  tif <- list.files(
    path = tmpdir,
    pattern = "*.tif",
    full.names = TRUE,
    recursive = TRUE
  )
  ##
  if ( length(tif) > 0 ) {
    parallel::mclapply(
      tif,
      function(tn){
        try(
          tiff::writeTIFF(
            what = tiff::readTIFF(tn),
            where = tn,
            compression = "deflate"
          ),
          silent = FALSE
        )
      }
    )
  }
  ##
  dir.create( file.path(output, "flowcam"), recursive = TRUE, showWarnings = FALSE )
  file.copy(
    from = file.path( tmpdir, "."),
    to = file.path( output, "flowcam", "."),
    recursive = TRUE
  )
  ##
  unlink( tmpdir, recursive = TRUE )
  ##
  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
