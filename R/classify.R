#' Classify `algae_traits` nad calculates densities
#'
#' @param algae_traits algae_traits
#' @param classifiers_constant constant temperature classifier
#' @param classifiers_increasing increasing temperature classifier
#' @param composition composition
#' @param exp_design experimental design
#' @param species_tracked species tracked
#'
#' @return `list` containing two objects:
#'    - `algae_traits` including species
#'    - `algae_densities` densities of the differenc particles identifieds
#' @export
#'
#' @md
#'
#' @examples
classify <- function(
  algae_traits,
  classifiers_constant,
  classifiers_increasing,
  composition,
  exp_design,
  species_tracked
){
  # 0. Load in experiment design and add to algae_traits df

  algae_traits <- dplyr::left_join(algae_traits, design, "bottle")

  # 2. Make a list of 32 dataframes: split morph_mvt based on species combination and temperature regime

  algae_traits_list <- split(x = algae_traits,
                             f = algae_traits$bottle,
                             drop = TRUE)

  # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers

  for (i in seq_along(algae_traits_list)) {

    df <- algae_traits_list[[i]]

    temperature_treatment <- unique(df$temperature) # either "constant" or "increasing"
    composition_id <- unique(df$composition) # a char between c_01 and c_16
    noNAs <- !rowSums(is.na(df)) > 0

    if (temperature_treatment == "constant") {
      pr <- predict(classifiers_constant[[composition_id]], df, probability = TRUE)
      df$species[noNAs] <- as.character(pr) # species prediction
      df$species_probability[noNAs] <- apply(attributes(pr)$probabilities, 1, max) # probability of each species prediction
      probabilities <- attributes(pr)$probabilities
      colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
      df <- cbind(df, probabilities)
    } else {
      pr <- predict(classifiers_increasing[[composition_id]], df, probability = TRUE)
      df$species[noNAs] <- as.character(pr) # species prediction
      df$species_probability[noNAs] <- apply(attributes(pr)$probabilities, 1, max)  # probability of each species prediction
      probabilities <- attributes(pr)$probabilities
      colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
      df <- cbind(df, probabilities)
    }
    algae_traits_list[[i]] <- df
  }

  # 4. Merge the 32 dfs back into a single df: algae_traits

  algae_traits <- purrr::reduce(algae_traits_list, dplyr::full_join)

  #############################################################
  # calculate species densities -------------------------------------------------------------------------
  #############################################################

  algae_density <- algae_traits %>%
    group_by(
      Date_Flowcam,
      species,
      bottle,
      composition,
      temperature,
      incubator,
      volume_imaged,
      dilution_factor,
      richness
    ) %>%
    summarise(count = n()) %>%
    mutate(density = count * dilution_factor / volume_imaged)


  # -----------------------------------------------------------------------------------------------------
  # add density = 0 for extinct species ------------------------------------------------------------


  comp_id <- unique(composition$composition)
  composition <- composition %>%
    dplyr::select(tidyselect::any_of(species_tracked))

  composition.list <- apply(composition, 1, function(x) {
    idx <- which(x == 1)
    names(idx)
  })
  names(composition.list) <- comp_id

  algae_density_list <- split(x = algae_density,
                              f = algae_density$bottle,
                              drop = T)

  for (i in seq_along(algae_density_list)) {
    df <- algae_density_list[[i]]
    ID <- unique(df$composition)
    idx <- which(!is.element(unlist(composition.list[ID]), df$species))
    if (length(idx) == 0) next
    for (j in idx) {
      new.entry <- tail(df, 1)
      new.entry$species <- composition.list[[ID]][j]
      new.entry$density <- 0
      new.entry$count <- 0
      df <- rbind(df, new.entry)
    }
    algae_density_list[[i]] <- df
  }

  algae_density <- do.call("rbind", algae_density_list) %>%
    filter(species %in% species_tracked)

  return(
    list(
      algae_traits = algae_traits,
      algae_density = algae_density
    )
  )
}
