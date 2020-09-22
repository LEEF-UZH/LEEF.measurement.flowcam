#' Check if data in raw data folder is OK
#'
#' @param input raw data folder containing flowcam data, i.e usually is \code{some/path/flowcam}
#'
#' @return \code{TRUE} if ok, \code{FALSE} or \code{list} of problems if not
#' @importFrom utils read.delim
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data_ok()
#' }
raw_data_ok <- function(input) {
  ok <- list()

  on.exit(
    if (all(unlist(ok))) {
      return(TRUE)
    } else {
      return(ok)
    }
  )

  # ok$flowcam_extract <- file.exists( file.path(input, "flowcam", "flowcam_extract.yml") )
  # ok$video_description <- file.exists( file.path(input, "flowcam", "video.description.txt") )

  ok$data_present <- length(
    list.dirs(
      path = file.path( input, "flowcam" ),
      full.names = FALSE
    )[-1]
  ) > 0

  return(ok)
}
