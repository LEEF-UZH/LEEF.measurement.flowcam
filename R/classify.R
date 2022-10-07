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
classify <- function(
  algae_traits,
  classifiers,
  exp_design,
  species_tracked,
  timestamp
){
  # 0. Load in experiment design and add to algae_traits df

  algae_traits <- dplyr::left_join(algae_traits, exp_design, "bottle")

  # 2. Make a list of 32 dataframes: split morph_mvt based on species combination and temperature regime

  algae_traits_list <- split(x = algae_traits,
                             f = algae_traits$bottle,
                             drop = TRUE)

  # 3. Predict species identities in the 32 dfs based on the 32 rf classifiers

  FlowCamClass <- function(classifier, df, noNAs){
    noNAs <- !rowSums(is.na(df)) > 0

    pr <- predict(classifier, df, probability = TRUE)

    df$species[noNAs] <- pr # species prediction
    df$species_probability[noNAs] <- apply(attributes(pr)$probabilities, 1, max) # probability of each species prediction

    probabilities <- attributes(pr)$probabilities
    colnames(probabilities) <- paste0(colnames(probabilities),"_prob")
    df <- cbind(df, probabilities)

    return(df)
  }

  for (i in seq_along(algae_traits_list)) {

    df <- algae_traits_list[[i]]

    TTreat <- unique(df$temperature_treatment) # either "constant" or "increasing"
    NTreat <- unique(df$nutrient_treatment) # either "constant" or "increasing"
    STreat <- unique(df$salt_treatment) # either "constant" or "increasing"


    if(TTreat=="constant" & NTreat=="constant" & STreat=="constant") {
      df <- FlowCamClass(classifiers$TC_NC_SC, df, noNAs)
    } else if(TTreat=="inreasing" & NTreat=="constant" & STreat=="constant") {
      df <- FlowCamClass(classifiers$TI_NC_SC, df, noNAs)
    } else if(TTreat=="constant" & NTreat=="inreasing" & STreat=="constant") {
      df <- FlowCamClass(classifiers$TC_NI_SC, df, noNAs)
    } else if(TTreat=="constant" & NTreat=="constant" & STreat=="inreasing") {
      df <- FlowCamClass(classifiers$TC_NC_SI, df, noNAs)
    } else if(TTreat=="inreasing" & NTreat=="inreasing" & STreat=="constant") {
      df <- FlowCamClass(classifiers$TI_NI_SC, df, noNAs)
    } else if(TTreat=="inreasing" & NTreat=="constant" & STreat=="inreasing") {
      df <- FlowCamClass(classifiers$TI_NC_SI, df, noNAs)
    } else if(TTreat=="constant" & NTreat=="inreasing" & STreat=="inreasing") {
      df <- FlowCamClass(classifiers$TC_NI_SI, df, noNAs)
    } else {
      df <- FlowCamClass(classifiers_TI_NI_SI, df, noNAs)
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
      temperature,
      incubator,
      volume_imaged,
      dilution_factor,
      nutrient_treatment,
      temperature_treatment,
      salt_treatment,
      replicate
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
    idx <- which(!is.element(species.tracked, df$species))
    if (length(idx) == 0) next
    for (j in idx) {
      new.entry <- tail(df, 1)
      new.entry$species <- species.tracked[j]
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
