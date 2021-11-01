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
#' @importFrom plyr join
#' @importFrom magrittr %>%
#' @importFrom stats predict
#' @importFrom utils read.csv
#'
#' @export
extractor_flowcam_prepare <- function(input, output) {
  message("########################################################")
  message("   preparing flowcam...")

  add_path <- file.path(output, "flowcam")
  dir.create(add_path, recursive = TRUE, showWarnings = FALSE)
  loggit::set_logfile(file.path(add_path, "flowcam.log"))

  ##
  processing <- file.path(normalizePath(output), "flowcam", paste0("EXTRACTING_PREPARE.FLOWCAM", ".PROCESSING"))
  error <- file.path(normalizePath(output), "flowcam", paste0("ERROR.EXTRACTING_PREPARE.FLOWCAM", ".ERROR"))
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

  trait_files <- list.files(
    path = flowcam_path,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  trait_files <- grep(
    pattern = "composition|experimental|dilution",
    trait_files,
    invert = TRUE,
    value = TRUE
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
    full.names = TRUE
  )

  if (length(trait_files) != length(metadata_files)) {
    message("ERROR - unequal number of trait and metadata files. Processing Aborted!!!")
    message("########################################################")
    return(invisible(FALSE))
  }

  if (length(trait_files) == 0) {
    unlink(processing)
    message("nothing to extract")
    message("########################################################")
    return(invisible(TRUE))
  }

  # additional files

  dilution_file <- file.path(flowcam_path, "flowcam_dilution.csv")

#############################################################
### <<< BEGIN SCRIPT ########################################
#############################################################

# Read accompanying files

dilution <- read.csv(dilution_file)

if (!all(c("bottle", "dilution_factor") %in% names(dilution))) {
  dilution <- read.csv(dilution_file, sep = ";")
}

#############################################################
#  Read in traits -------------------------------------------------------------------
#############################################################

# read in all data frames
algae_traits <- lapply(trait_files, data.table::fread)

# bind data frames to one large data frame
algae_traits <- dplyr::bind_rows(algae_traits)

# rename columns
colnames(algae_traits) <- make.names(colnames(algae_traits))
colnames(algae_traits) <- gsub("\\.$", "", colnames(algae_traits))
colnames(algae_traits) <- gsub("\\.\\.", ".", colnames(algae_traits))
colnames(algae_traits) <- gsub("\\.", "_", colnames(algae_traits))

names(algae_traits)[names(algae_traits) == "Date"] <- "Date_Flowcam"
names(algae_traits)[names(algae_traits) == "Timestamp"] <- "Timestamp_Flowcam"

# extract information on date and microcosm from the column "Image_File"

algae_traits$temp_ID <- gsub(x = algae_traits$Image_File, pattern = "\\_", replacement = " ")
algae_traits$bottle <- sapply(strsplit(algae_traits$temp_ID, " "), "[", 1)

algae_traits <- subset(algae_traits, select = -temp_ID)


#############################################################
# Read in meta-data --------------------------------------------------------------------
#############################################################


# read in the files with the meta-data (i.e. txt-file in each subfolder)




# extract information on bottle from the name of the file (file names are in metadata_files)
# read in the lines of each txt-file
# lines have parameter and value separated by ":"
# split at ":" and get parameter and value

meta_data <- lapply(
  metadata_files,
  function(fn) {
    bottle <- strsplit(
      basename(fn),
      "_"
    )[[1]][[1]]

    file_data <- readLines(fn)
    file_data <- gsub("\\t", "", file_data)
    file_data <- strsplit(file_data, "\\:")
    parameter <- trimws(sapply(file_data, "[", 1))
    value <- trimws(sapply(file_data, "[", 2))
    parameter <- parameter[!is.na(value)]
    value <- value[!is.na(value)]
    meta_data <- data.frame(bottle = bottle,
                            parameter = parameter,
                            value = value)

    return(meta_data)
  }
)
meta_data <- do.call(rbind, meta_data)

# get data on volume imaged
volume_imaged <- meta_data[meta_data$parameter == "Fluid Volume Imaged", ]
volume_imaged$value <- sapply(strsplit(volume_imaged$value, " "), "[", 1)
volume_imaged$value <- as.numeric(volume_imaged$value)
volume_imaged <- subset(volume_imaged, select = -parameter)
names(volume_imaged)[names(volume_imaged) == "value"] <- "volume_imaged"

algae_traits <- plyr::join(algae_traits, volume_imaged, by = "bottle")



# !!! The next line won't be needed in final scriupt I think !!! Ask Romana...
algae_traits$bottle <- ifelse(
  as.numeric(algae_traits$bottle) < 10,
  paste0("b_0", algae_traits$bottle),
  paste0("b_", algae_traits$bottle)
)

algae_traits <- plyr::join(algae_traits, dilution, by = "bottle")

#############################################################
### >>> END SCRIPT ##########################################
#############################################################


# SAVE --------------------------------------------------------------------

  saveRDS(
    algae_traits,
    file = file.path(add_path, "algae_traits_prepared.rds")
  )

  utils::write.csv(
    meta_data,
    file = file.path(add_path, "algae_metadata.csv"),
    row.names = FALSE
  )

# Finalize ----------------------------------------------------------------

  unlink(processing)
  message("   done")
  message("########################################################")

  invisible(TRUE)
}
