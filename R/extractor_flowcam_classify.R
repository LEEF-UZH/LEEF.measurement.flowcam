#' Preprocessor flowcam data
#'
#' extract data from all \code{*_classes_*_data.csv} files
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom data.table fread
#' @importFrom yaml read_yaml yaml.load
#' @importFrom utils write.csv
#' @importFrom dplyr left_join group_by summarise mutate n select filter full_join
#' @importFrom purrr reduce
#' @importFrom plyr join ldply
#' @importFrom magrittr %>%
#' @importFrom stats predict
#' @importFrom utils read.csv
#' @import e1071
#'
#' @export
extractor_flowcam_classify <- function(input, output) {
  message("########################################################")
  message("   classifying flowcam...")

  add_path <- file.path(output, "flowcam")
  dir.create(add_path, recursive = TRUE, showWarnings = FALSE)
  loggit::set_logfile(file.path(add_path, "flowcam.log"))

  load_parameter(file.path(input, "flowcam", "flowcam.yml"))

  ##
  processing <- file.path(normalizePath(output), "flowcam", paste0("EXTRACTING_CLASSIFY.FLOWCAM", ".PROCESSING"))
  error <- file.path(normalizePath(output), "flowcam", paste0("ERROR.EXTRACTING_CLASSIFY.FLOWCAM", ".ERROR"))
  on.exit({
    if (file.exists(processing)) {
      unlink(processing)
      file.create(error)
    }
  }
  )
  file.create(processing)
  ##

  # Get flowcam directory names ------------------------------------------------------

  flowcam_path <- file.path(input, "flowcam")

  algae_traits_file <- file.path(add_path, "algae_traits_filtered.rds")

  # additional files

  design_file <- file.path(flowcam_path, "experimental_design.csv")

  # composition_file <- file.path(flowcam_path, "compositions.csv")

  # the classifiers for increasing temperatures will have to be updated during experiment!!

  # Read classifiers into list ----------------------------------------------


  dir_classifiers <- file.path(flowcam_path, par_classifiers())

  class_files <- list.files(dir_classifiers, pattern = "\\.rds$", full.names = TRUE)
  classifiers <- lapply(
    class_files,
    readRDS
  )
  names(classifiers) <- tools::file_path_sans_ext(basename(class_files))
  classifiers$comment <- readLines(file.path(dir_classifiers, "README.txt"))


  # classify ----------------------------------------------------------------


  result <- classify_LEEF_2(
    algae_traits = readRDS(algae_traits_file),
    classifiers = classifiers,
    exp_design = read.csv(design_file),
    species_tracked = par_species_tracked(),
    timestamp = yaml::read_yaml(file.path(input,  "flowcam", "sample_metadata.yml"))$timestamp
  )


  # SAVE --------------------------------------------------------------------

  #
  utils::write.csv(
    result$algae_traits,
    file = file.path(add_path, "algae_traits.csv"),
    row.names = FALSE
  )
  #
  utils::write.csv(
    result$algae_density,
    file = file.path(add_path, "algae_density.csv"),
    row.names = FALSE
  )
  #
  to_copy <- grep(
    list.files(
      file.path(input, "flowcam"),
      full.names = TRUE
    ),
    pattern = "_classifiers_",
    invert = TRUE,
    value = TRUE
  )
  file.copy(
    from = to_copy,
    to = file.path(output, "flowcam", "")
  )

  # Finalize ----------------------------------------------------------------

  unlink(processing)
  message("   done")
  message("########################################################")

  invisible(TRUE)
}
