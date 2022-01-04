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
  composition_file <- file.path(flowcam_path, "compositions.csv")

  # the classifiers for increasing temperatures will have to be updated during experiment!!
  name_constant <- file.path(flowcam_path, par_classifier_constant())
  name_increasing <- file.path(flowcam_path, par_classifier_increasing())

  #############################################################
  ### <<< BEGIN SCRIPT ########################################
  #############################################################

  # design <- read.csv(design_file)
  # comps <- read.csv(composition_file)

  # algae_traits <- readRDS(algae_traits_file)

  #############################################################
  # Classification  ------------------------------------------------------------------------
  #############################################################

  # next steps: classify each particle in algae_traits
  # then calculate density for each species (mutiply the count for each species with dilution_factor
  # and divide by volume_imaged)
  # in the end we want to keep the dataframe algae_traits (then containing a column with species identity)
  # the dataframe meta_data
  # and the dataframe that contains the density/ml for each species


  # Predict species identities with svm --------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------

  # algae_traits <- result$algae_traits
  # algae_density <- result$algae_density

  ###
  ### This has been re-located into the function `classify`
  ###
  # # 1. Load in random classifiers (rds)
  #
  # #classifier files
  #
  #
  # classifiers_constant <- readRDS(name_constant)
  # classifiers_increasing <- readRDS(name_increasing)
  #
  # # 2. Make a list of 32 dataframes: split morph_mvt based on species combination and temperature regime
  #
  # algae_traits_list <- split(x = algae_traits,
  #                            f = algae_traits$bottle,
  #                            drop = TRUE)
  #
  # # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers
  #
  # for (i in seq_along(algae_traits_list)) {
  #
  #   df <- algae_traits_list[[i]]
  #
  #   temperature_treatment <- unique(df$temperature) # either "constant" or "increasing"
  #   composition_id <- unique(df$composition) # a char between c_01 and c_16
  #   noNAs <- !rowSums(is.na(df)) > 0
  #
  #   if (temperature_treatment == "constant") {
  #     pr <- predict(classifiers_constant[[composition_id]], df, probability = TRUE)
  #     df$species[noNAs] <- as.character(pr) # species prediction
  #     df$species_probability[noNAs] <- apply(attributes(pr)$probabilities, 1, max) # probability of each species prediction
  #     probabilities <- attributes(pr)$probabilities
  #     colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
  #     df <- cbind(df, probabilities)
  #   } else {
  #     pr <- predict(classifiers_increasing[[composition_id]], df, probability = TRUE)
  #     df$species[noNAs] <- as.character(pr) # species prediction
  #     df$species_probability[noNAs] <- apply(attributes(pr)$probabilities, 1, max)  # probability of each species prediction
  #     probabilities <- attributes(pr)$probabilities
  #     colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
  #     df <- cbind(df, probabilities)
  #   }
  #   algae_traits_list[[i]] <- df
  # }
  #
  # # 4. Merge the 32 dfs back into a single df: algae_traits
  #
  # algae_traits <- purrr::reduce(algae_traits_list, dplyr::full_join)
  #
  # #############################################################
  # # calculate species densities -------------------------------------------------------------------------
  # #############################################################
  #
  # algae_density <- algae_traits %>%
  #   group_by(
  #     Date_Flowcam,
  #     species,
  #     bottle,
  #     composition,
  #     temperature,
  #     incubator,
  #     volume_imaged,
  #     dilution_factor,
  #     richness
  #   ) %>%
  #   summarise(count = n()) %>%
  #   mutate(density = count * dilution_factor / volume_imaged)
  #
  #
  # # -----------------------------------------------------------------------------------------------------
  # # add density = 0 for extinct species ------------------------------------------------------------
  #
  #
  # comp_id <- unique(comps$composition)
  # comps <- comps %>%
  #   dplyr::select(tidyselect::any_of(par_species_tracked()))
  #
  # comps.list <- apply(comps, 1, function(x) {
  #   idx <- which(x == 1)
  #   names(idx)
  # })
  # names(comps.list) <- comp_id
  #
  # algae_density_list <- split(x = algae_density,
  #                             f = algae_density$bottle,
  #                             drop = T)
  #
  # for (i in seq_along(algae_density_list)) {
  #   df <- algae_density_list[[i]]
  #   ID <- unique(df$composition)
  #   idx <- which(!is.element(unlist(comps.list[ID]), df$species))
  #   if (length(idx) == 0) next
  #   for (j in idx) {
  #     new.entry <- tail(df, 1)
  #     new.entry$species <- comps.list[[ID]][j]
  #     new.entry$density <- 0
  #     new.entry$count <- 0
  #     df <- rbind(df, new.entry)
  #   }
  #   algae_density_list[[i]] <- df
  # }
  #
  # algae_density <- do.call("rbind", algae_density_list) %>%
  #   filter(species %in% par_species_tracked())

  # result$algae_traits$timestamp <- timestamp
  # result$algae_density$timestamp <- timestamp

  result <- LEEF.measurement.flowcam::classify(
    algae_traits = readRDS(algae_traits_file),
    classifiers_constant = readRDS(name_constant),
    classifiers_increasing = readRDS(name_increasing),
    composition = read.csv(composition_file),
    exp_design = read.csv(design_file),
    species_tracked = par_species_tracked(),
    timestamp = yaml::read_yaml(file.path(input,  "flowcam", "sample_metadata.yml"))$timestamp

  )

  #############################################################
  ### >>> END SCRIPT ##########################################
  #############################################################

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
