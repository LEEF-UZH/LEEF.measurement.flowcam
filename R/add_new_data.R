#' Check if data in input folder is OK and move to raw data folder
#'
#' @param input The folder, where a folder \code{flowcam} is located which
#'   contains the new files.
#' @param output A folder, which contains a subfolder called \code{flowcam}, i.e.
#'   the usually the raw data folder, into which the fioles will be moved to.
#'
#' @return a \code{list} which contains the individual reseults for each file.
#'   \code{TRUE} if moved, \code{FALSE} if an error occured. Details of the eror
#'   re in the error files in the \code{input/flowcam} directory.
#' @importFrom parallel mclapply
#' @export
#'
add_new_data <- function(input, output) {
  ##
  dir.create(
    file.path(output, "flowcam"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Copy ALL other files ----------------------------------------------------

#   others <- grep(
#     list.files(
#       path = input,
#       full.names = TRUE
#     ),
#     pattern='.cxd',
#     invert=TRUE,
#     value=TRUE
#   )
#   file.copy(
#     from = others,
#     to = file.path(output, "bemovi"),
#     overwrite = TRUE
#   )
#   unlink( others )

  # Check and move folder ------------------------------------------------------

  folder <- list.dirs(
    path = input,
    full.names = FALSE
  )[-1]

  ##
  ok <- parallel::mclapply(
    folder,
    function(f) {
      processing <- file.path(input, paste0("CHECKING.", f, ".CHECKING"))
      error <- file.path(input, paste0("ERROR.", f, ".txt"))

      on.exit(
        {
          if (file.exists(processing)) {
            unlink(processing)
            capture.output(print(result), file = error)
          }
        }
      )
      ##
      file.create( processing )
      ##
      message("checking ", f)
      result <- list(
        ok = TRUE
      )

      files <- list.files(
        file.path(input, f)
      )

      # Check if file exist ----------------------------------------------------------


      result$cal <-        length( grep("^cal_image_([0-9]{6})\\.tif", files) ) >= 1
      result$lst <-        length( grep(paste0("^", f, "\\.lst"),      files) ) == 1
      result$edg <-        length( grep(paste0("^", f, "\\.edg"),      files) ) == 1
      result$ctx <-        length( grep(paste0("^", f, "\\.ctx"),      files) ) == 1
      result$csv <-        length( grep(paste0("^", f, "\\.csv"),      files) ) == 1
      result$summary <-    length( grep(paste0("^", f, "_summary\\.csv"), files) ) == 1
      result$runsummary <- length( grep(paste0("^", f, "_run_summary\\.txt"), files) ) == 1

      tif    <- grep( paste0("^", f, "_([0-9]{6})\\.tif"), files, value = TRUE)
      bintif <- grep( paste0("^", f, "_([0-9]{6})\\_bin.tif"), files, value = TRUE)

      result$no_tif_eq_no_bin.tif <- length(tif) == length(bintif)

      tif <- gsub("\\.tif$", "", tif)
      bintif <- gsub("\\_bin.tif$", "", bintif)
      result$tifnames <- all.equal(tif, bintif)

      result$tifno <- length(tif) == as.numeric(max(gsub(paste0(f, "_"), "", tif)))

      result$ok <- all(unlist(result))

      if ( result$ok ) {
        file.copy(
          from = file.path(input, f),
          to = file.path(output, "flowcam"),
          recursive = TRUE,
          overwrite = TRUE
        )
        unlink( file.path(input, f), recursive = TRUE )
        unlink(processing)
      }
      return(result)
    }
  )
  names(ok) <- folder
  return(ok)
}
