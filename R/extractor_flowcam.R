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
#' @importFrom	yaml read_yaml yaml.load
#'
#' @export
extractor_flowcam <- function( input, output ) {
  message("\n########################################################\n")
  message("\nExtracting flowcam...\n")


# Get flowcam directory names ------------------------------------------------------

  flowcam_path <- file.path( input, "flowcam" )
  trait_files <- list.files(
    path = flowcam_path,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = FALSE
  )
  trait_files <- grep(
    pattern = "_summary.csv",
    trait_files,
    value = TRUE,
    invert = TRUE
  )

  metadata_files <- list.files(
    path = flowcam_path,
    pattern = "_summary\\.txt$",
    recursive = TRUE,
    full.names = FALSE
  )

  if (length(trait_files) != length(metadata_files)) {
    message("ERROR - unequal number of trait and metadata files. Processing Aborted!!!\n")
    message("\n########################################################\n")
    return(invisible(FALSE))
  }

  if (length(trait_files) == 0) {
    message("nothing to extract\n")
    message("\n########################################################\n")
    return(invisible(FALSE))
  }

  # read in traits ----------------------------------------------------------

  traits <- lapply(
    file.path(flowcam_path, trait_files),
    data.table::fread
  )

  # traits <- dplyr::bind_rows(traits)
  traits <- do.call( rbind.data.frame, traits )

  colnames(traits) <- make.names(colnames(traits))
  colnames(traits) <- gsub("\\.$", "", colnames(traits))
  colnames(traits) <- gsub("\\.\\.", "_", colnames(traits))
  colnames(traits) <- gsub("\\.", "_", colnames(traits))


  # extract information on bottle from the column "Image_file" --------

  traits$bottle <- sapply(
    traits$Image_File,
    function(x) {
      x <- strsplit(
        x,
        split = "_"
      )
      x[[1]][[1]]
    }
  )

# read in metadata --------------------------------------------------------

  metadata <- lapply(
    metadata_files,
    function(x){
      bottle <- dirname(x)

      md <- readLines( file.path( flowcam_path, x ) )
      md <- grep("\t", md, value = TRUE)
      md <- gsub( "\t", "   ", md)
      md <-  yaml::yaml.load( md  )

      md <- data.frame(
        bottle = bottle,
        parameter = names(unlist(md)),
        value = unlist(md, use.names = FALSE)
      )

      return(md)
      }
    )
  metadata <- do.call( rbind.data.frame, metadata )

  # add volume_imaged to traits ---------------------------------------------

  volume_imaged <- subset(
    metadata[c("bottle", "value")],
    subset =  metadata$parameter == "Fluid Volume Imaged"
  )
  names(volume_imaged)[names(volume_imaged)=="value"] <- "volume_imaged"

  traits <- merge(
      x = traits,
      y = volume_imaged,
      by = c("bottle"),
      all.x = TRUE,
      all.y = FALSE
  )


# append `_flowcam` to `Date` and `Timestamp` -----------------------------

  names(traits)[ names(traits) == "Date" ] <- "Date_flowcam"
  names(traits)[ names(traits) == "Timestamp" ] <- "Timestamp_flowcam"

# SAVE --------------------------------------------------------------------

  add_path <- file.path( output, "flowcam" )
  dir.create( add_path, recursive = TRUE, showWarnings = FALSE )
  #
  write.csv(
    traits,
    file = file.path(add_path, "algae_traits.csv"),
    row.names = FALSE
  )
  #
  write.csv(
    metadata,
    file = file.path(add_path, "algae_metadata.csv"),
    row.names = FALSE
  )
  file.copy(
    from = file.path(input, "sample_metadata.yml"),
    to = file.path(output, "sample_metadata.yml")
  )

# Finalize ----------------------------------------------------------------

  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
