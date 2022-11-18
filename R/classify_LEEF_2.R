#' Classify `algae_traits` nad calculates densities
#'
#' @param algae_traits algae_traits
#' @param classifiers list with classifiers as elements
#' @param exp_design experimental design
#' @param species_tracked species tracked
#' @param timestamp timestamp to be used to stamp the classified data
#'
#' @return `list` containing two objects:
#'    - `algae_traits` including species
#'    - `algae_densities` densities of the differenc particles identifieds
#' @export
#'
#' @md
#'
#' @examples
classify_LEEF_2 <- function(
  algae_traits,
  classifiers,
  exp_design,
  species_tracked,
  timestamp
){
  # 0. Load in experiment design and add to algae_traits df

  # algae_traits <- dplyr::left_join(algae_traits, exp_design, "bottle")

  # 2. Make a list of 32 dataframes: split morph_mvt based on species combination and temperature regime

  algae_traits_list <- split(x = algae_traits,
                             f = algae_traits$bottle,
                             drop = TRUE)

  # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers

  FlowCamClass <- function(classifier, df){
    noNAs <- !rowSums(is.na(df)) > 0

    pr <- predict(classifier, df, probability = TRUE)
    df$species[noNAs] <- as.character(pr) # species prediction

    df$species_probability[noNAs] <- apply(attributes(pr)$probabilities, 1, max) # probability of each species prediction

    probabilities <- attributes(pr)$probabilities
    colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
    df <- cbind(df, probabilities)

    return(df)
  }

  for (i in seq_along(algae_traits_list)) {

    df <- algae_traits_list[[i]]

    x <- unique(df$temperature) # either "constant" or "increasing"
    if (x == "increasing") {
      TTreat <- "TI"
    } else if (x == "constant") {
      TTreat <- "TC"
    } else {
      stop("Undefined value for `temperature` in experimental design table!")
    }

    x <- unique(df$resources) # either "constant" or "increasing"
    if (x == "increasing") {
      NTreat <- "NI"
    } else if (x == "constant") {
      NTreat <- "NC"
    } else {
      stop("Undefined value for `resources` in experimental design table!")
    }

    x <- unique(df$salinity) # either "constant" or "increasing"
    if (x == "increasing") {
      STreat <- "SI"
    } else if (x == "constant") {
      STreat <- "SC"
    } else {
      stop("Undefined value for `salinity` in experimental design table!")
    }

    classifier_name <- paste(TTreat, NTreat, STreat, sep = "_")

    df <- FlowCamClass(classifiers[[classifier_name]], df)

    algae_traits_list[[i]] <- df
  }

  # 4. Merge the 32 dfs back into a single df: algae_traits

  algae_traits <- suppressMessages(purrr::reduce(algae_traits_list, dplyr::full_join))

  #############################################################
  # calculate species densities -------------------------------------------------------------------------
  #############################################################

  algae_density <- algae_traits %>%
    group_by(
      Date_Flowcam,
      species,
      bottle,
      temperature,
      salinity,
      replicate,
      incubator,
      volume_imaged,
      dilution_factor,
      resources
      ) %>%
    summarise(count = n()) %>%
    mutate(density = count * dilution_factor / volume_imaged)


  # -----------------------------------------------------------------------------------------------------
  # add density = 0 for extinct species ------------------------------------------------------------


  # comp_id <- unique(composition$composition)
  # composition <- composition %>%
  #   dplyr::select(tidyselect::any_of(species_tracked))
  #
  # composition.list <- apply(composition, 1, function(x) {
  #   idx <- which(x == 1)
  #   names(idx)
  # })
  # names(composition.list) <- comp_id

  algae_density_list <- split(x = algae_density,
                              f = algae_density$bottle,
                              drop = TRUE)


  # for (i in seq_along(algae_density_list)) {
  #   df <- algae_density_list[[i]]
  #   ID <- unique(df$composition)
  #   idx <- which(!is.element(unlist(composition.list[ID]), df$species))
  #   if (length(idx) == 0) next
  #   for (j in idx) {
  #     new.entry <- tail(df, 1)
  #     new.entry$species <- composition.list[[ID]][j]
  #     new.entry$density <- 0
  #     new.entry$count <- 0
  #     df <- rbind(df, new.entry)
  #   }
  #   algae_density_list[[i]] <- df
  # }

  for (i in seq_along(algae_density_list)) {
    df <- algae_density_list[[i]]
    idx <- which(!is.element(par_species_tracked(), df$species))
    if (length(idx) == 0) next
    for (j in idx) {
      new.entry <- tail(df, 1)
      new.entry$species <- par_species_tracked()[j]
      new.entry$density <- 0
      new.entry$count <- 0
      df <- rbind(df, new.entry)
    }
    algae_density_list[[i]] <- df
  }

  algae_density <- do.call("rbind", algae_density_list) %>%
    filter(species %in% species_tracked)

  algae_traits$timestamp <- timestamp
  algae_density$timestamp <- timestamp

  return(
    list(
      algae_traits = algae_traits,
      algae_density = algae_density
    )
  )
}
