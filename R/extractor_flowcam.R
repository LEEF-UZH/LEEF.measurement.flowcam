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
#' @import randomForest
#'
#' @export
extractor_flowcam <- function( input, output ) {
  message("\n########################################################\n")
  message("\nExtracting flowcam...\n")

  add_path <- file.path( output, "flowcam" )
  dir.create( add_path, recursive = TRUE, showWarnings = FALSE )

  ##
  processing <- file.path(normalizePath(output), "flowcam", paste0("EXTRACTING.FLOWCAM", ".PROCESSING"))
  error <- file.path(normalizePath(output), "flowcam", paste0("ERROR.EXTRACTING.FLOWCAM", ".ERROR"))
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

# Get flowcam directory names ------------------------------------------------------

  flowcam_path <- file.path( input, "flowcam" )
  trait_files <- list.files(
    path = flowcam_path,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = FALSE
  )
  trait_files <- grep("composition|experimental|dilution", trait_files, invert = TRUE, value = TRUE)
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
    unlink(processing)
    message("nothing to extract\n")
    message("\n########################################################\n")
    return(invisible(TRUE))
  }

  # read in algae_traits ----------------------------------------------------------

  algae_traits <- lapply(
    file.path(flowcam_path, trait_files),
    data.table::fread
  )

  # algae_traits <- dplyr::bind_rows(algae_traits)
  algae_traits <- do.call( rbind.data.frame, algae_traits )

  colnames(algae_traits) <- make.names(colnames(algae_traits))
  colnames(algae_traits) <- gsub("\\.$", "", colnames(algae_traits))
  colnames(algae_traits) <- gsub("\\.\\.", "_", colnames(algae_traits))
  colnames(algae_traits) <- gsub("\\.", "_", colnames(algae_traits))


  # extract information on bottle from the column "Image_file" --------

  algae_traits$bottle <- sapply(
    algae_traits$Image_File,
    function(x) {
      x <- strsplit(
        x,
        split = "_"
      )
      x[[1]][[1]]
    }
  )
  algae_traits$bottle <- sprintf("b_%02d", as.integer(algae_traits$bottle))

# read in metadata --------------------------------------------------------

  metadata <- lapply(
    metadata_files,
    function(x){
      bottle <- as.integer(dirname(x))

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

  # add volume_imaged to algae_traits ---------------------------------------------

  volume_imaged <- subset(
    metadata[c("bottle", "value")],
    subset =  metadata$parameter == "Fluid Volume Imaged"
  )
  names(volume_imaged)[names(volume_imaged)=="value"] <- "volume_imaged"
	volume_imaged$volume_imaged <- gsub(
		" ml",
		"",
		volume_imaged$volume_imaged
	)

  algae_traits <- merge(
      x = algae_traits,
      y = volume_imaged,
      by = c("bottle"),
      all.x = TRUE,
      all.y = FALSE
  )


# append `_flowcam` to `Date` and `Timestamp` and add `timestamp` ------------------

  names(algae_traits)[ names(algae_traits) == "Date" ] <- "Date_flowcam"
  names(algae_traits)[ names(algae_traits) == "Timestamp" ] <- "Timestamp_flowcam"
  timestamp <- yaml::read_yaml(file.path(input, "flowcam", "sample_metadata.yml"))$timestamp
  algae_traits <- cbind(timestamp = timestamp, algae_traits)
  metadata <- cbind(timestamp = timestamp, metadata)



# Species ID --------------------------------------------------------------

  design <- read.csv(file.path(input, "flowcam", "experimental_design.csv"))
  comps <- read.csv(file.path(input, "flowcam", "compositions.csv"))
  species.tracked <- c("Chlamydomonas", "Cosmarium", "Cryptomonas", "Desmodesmus", "Dexiostoma",
                       "Loxocephallus", "Monoraphidium", "Staurastrum1", "Staurastrum2", "Tetrahymena",
                       "airbubbles","ColpidiumVacuoles","Debris","OtherCiliate","ChlamydomonasClumps",
                       "Coleps_irchel", "Coleps_viridis", "Colpidium")
  dilution <- read.csv(file.path(input, "flowcam", "flowcam_dilution.csv"))
  algae_traits <- plyr::join(algae_traits, dilution, by = "bottle")


  # !!! The next line won't be needed in final scriupt I think !!! Ask Romana...

  algae_traits$bottle <- ifelse(algae_traits$bottle<10, paste0("b_0",algae_traits$bottle),paste0("b_",algae_traits$bottle))

  algae_traits <- dplyr::left_join(algae_traits, design, "bottle")

  # 1. Load in random classifiers (rds)

  classifiers_constant <- readRDS(
    file.path(
      input,
      "flowcam",
      "flowcam_classifiers_18c.rds"
    )
  )

  classifiers_increasing <- readRDS(
    file.path(
      input,
      "flowcam",
      "flowcam_classifiers_increasing_best_available.rds"
    )
  )

  # 2. Make a list of 32 dataframes: split morph_mvt based on species combination and temperature regime

  algae_traits_list <- split(
    x = algae_traits,
    f = algae_traits$bottle,
    drop = TRUE
  )

  # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers

  for(i in 1:length(algae_traits_list)){

    df <- algae_traits_list[[i]]

    temperature_treatment <- unique(df$temperature) # either "constant" or "increasing"
    composition_id <- unique(df$composition) # a char between c_01 and c_16

    if (temperature_treatment == "constant"){
      df$species <- predict(classifiers_constant[[composition_id]], df) # species prediction
      df$species_probability <- apply(predict(classifiers_constant[[composition_id]], df, type = "prob"),
                                      1,max) # probability of each species prediction
    } else {
      df$species <- predict(classifiers_increasing[[composition_id]], df) # species prediction
      df$species_probability <- apply(predict(classifiers_increasing[[composition_id]], df, type = "prob"),
                                      1,max) # probability of each species prediction
    }
    algae_traits_list[[i]] <- df
  }

  # 4. Merge the 32 dfs back into a single df: algae_traits

  algae_traits <- do.call("rbind", algae_traits_list)

# calculate species densities ---------------------------------------------

  algae_traits$volume_imaged <- as.numeric(algae_traits$volume_imaged)
  algae_density <- algae_traits %>%
    dplyr::group_by(timestamp,    species, bottle, composition, temperature, incubator, volume_imaged, dilution_factor, richness) %>%
#   dplyr::group_by(Date_Flowcam, species, bottle, composition, temperature, incubator, volume_imaged, dilution_factor, richness) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(density = count * dilution_factor / volume_imaged)

# add density = 0 for extinct species -------------------------------------

  comp_id <- unique(comps$composition)
  comps <- comps %>%
    dplyr::select(tidyselect::any_of(species.tracked))

  comps.list <- apply(
    comps, 1, function(x){
      idx <- which(x==1)
      names(idx)
    }
  )
  names(comps.list) <- comp_id

  algae_density_list <- split(x = algae_density,
                              f = algae_density$bottle,
                              drop = T)

  for(i in 1:length(algae_density_list)){

    df <- algae_density_list[[i]]
    ID <- unique(df$composition)
    idx <- which(!is.element(unlist(comps.list[ID]), df$species))
    if(length(idx)==0) next
    for(j in idx){
      new.entry <- tail(df,1)
      new.entry$species <- comps.list[ID][[1]][j]
      new.entry$density <- 0
      df <- rbind(df, new.entry)
    }
    algae_density_list[[i]] <- df
  }

  algae_density <- do.call("rbind", algae_density_list) %>%
    dplyr::filter(species %in% species.tracked)

# SAVE --------------------------------------------------------------------

  #
  utils::write.csv(
    algae_traits,
    file = file.path(add_path, "algae_traits.csv"),
    row.names = FALSE
  )
  #
  utils::write.csv(
    algae_density,
    file = file.path(add_path, "algae_density.csv"),
    row.names = FALSE
  )
  #
  utils::write.csv(
    metadata,
    file = file.path(add_path, "algae_metadata.csv"),
    row.names = FALSE
  )
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
  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
