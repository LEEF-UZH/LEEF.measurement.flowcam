#' Preprocessor flowcam data
#'
#' extract data from all \code{*_classes_*_data.csv} files
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom data.table fread
#' @importFrom yaml read_yaml yaml.load
#' @importFrom utils write.csv
#' @importFrom dplyr left_join group_by summarise mutate n select filter
#' @importFrom plyr join ldply
#' @importFrom magrittr %>%
#' @importFrom stats predict
#' @importFrom utils read.csv
#' @import randomForest
#' @import loggit
#'
#' @export
extractor_flowcam <- function(input, output) {
  add_path <- file.path(output, "flowcam")
  dir.create(
    add_path,
    recursive = TRUE,
    showWarnings = FALSE
  )
  loggit::set_logfile(file.path(add_path, "flowcam.log"))

  message("########################################################")
  message("Extracting flowcam...")

  if ( length( list.files( file.path(input, "flowcam") ) ) == 0 ) {
    message("Empty or missing flowcam directory - nothing to do.")
    message("done")
    message("########################################################")
    return(invisible(TRUE))
  }

  ##
  processing <- file.path(normalizePath(output), "flowcam", paste0("EXTRACTING.FLOWCAM", ".PROCESSING"))
  error <- file.path(normalizePath(output), "flowcam", paste0("ERROR.EXTRACTING.FLOWCAM", ".ERROR"))
  on.exit({
      if (file.exists(processing)) {
        unlink(processing)
        file.create(error)
      }
    }
  )
  file.create(processing)
  ##


#  Run functions --------------------------------------------------------------------

  extractor_flowcam_prepare(input, output)
  extractor_flowcam_filter(input, output)
  extractor_flowcam_classify(input, output)

# Finalize ----------------------------------------------------------------

  unlink(processing)
  message("done")
  message("########################################################")

  invisible(TRUE)
}
