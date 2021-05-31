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
#' @importFrom plyr join ldply
#' @importFrom magrittr %>%
#' @importFrom stats predict
#' @importFrom utils read.csv
#'
#' @export
extractor_flowcam_filter <- function(input, output) {
  message("\n########################################################\n")
  message("\nExtracting flowcam...\n")

  add_path <- file.path(output, "flowcam")
  dir.create(add_path, recursive = TRUE, showWarnings = FALSE)

  ##
  processing <- file.path(normalizePath(output), "flowcam", paste0("EXTRACTING_FILTER.FLOWCAM", ".PROCESSING"))
  error <- file.path(normalizePath(output), "flowcam", paste0("ERROR.EXTRACTING_FILTER.FLOWCAM", ".ERROR"))
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


  algae_traits_file <- file.path(add_path, "algae_traits_prepared.rds")

#############################################################
### <<< BEGIN SCRIPT ########################################
#############################################################



#############################################################
#  Read in traits -------------------------------------------------------------------
#############################################################

algae_traits <- readRDS(algae_traits_file)

#############################################################
# extractor_filter   ########################################
#############################################################

#############################################################
# Filter out repeated  images  ------------------------------------------------------------------------
#############################################################

algae_traits <- algae_traits %>%
  mutate(Area_x = ifelse(Capture_X < 325, "X1",
                         ifelse(Capture_X < 650, "X2",
                                ifelse(Capture_X < 975, "X3", "X4"))),
         Area_y = ifelse(Capture_Y < 250, "Y1",
                         ifelse(Capture_Y < 500, "Y2",
                                ifelse(Capture_Y < 750, "Y3", "Y4")))) %>%
  mutate(Subarea = interaction(Area_x, Area_y))

algae_traits_list <- split(x = algae_traits,
                           f = algae_traits$bottle,
                           drop = T)

atl_filtered <- lapply(algae_traits_list, function(df) {

  # df <- algae_traits_list[[i]]
  df_split <- split(df, f = df$Subarea)

  df_split <- lapply(df_split, function(sub_df) {

    dists <- as.matrix(dist(sub_df %>% dplyr::select(Capture_Y, Capture_X)))
    dists[upper.tri(dists, diag = T)] <- NA

    size.ratios <- outer(sub_df$Area_ABD, sub_df$Area_ABD, FUN = "/")
    size.ratios[upper.tri(size.ratios, diag = T)] <- NA

    aspect.ratios.ratios <- outer(sub_df$Aspect_Ratio, sub_df$Aspect_Ratio, FUN = "/")
    aspect.ratios.ratios[upper.tri(aspect.ratios.ratios, diag = T)] <- NA

    perimeter.ratios <- outer(sub_df$Perimeter, sub_df$Perimeter, FUN = "/")
    perimeter.ratios[upper.tri(perimeter.ratios, diag = T)] <- NA

    idx <- which(dists <= sqrt(2) &
                   0.9 < size.ratios & size.ratios < 1.1 &
                   0.9 < perimeter.ratios & perimeter.ratios < 1.1 &
                   0.9 < aspect.ratios.ratios & aspect.ratios.ratios < 1.1,
                 arr.ind = TRUE)

    rows <- unique(c(idx[, 1], idx[, 2]))
    IDs <- unname(unlist(sub_df[rows, "Particle_ID"]))

    sub_df %>% dplyr::filter(!is.element(Particle_ID, IDs))
  })

  do.call("rbind", df_split)
})

algae_traits <- do.call("rbind", atl_filtered)

#############################################################
### >>> END SCRIPT ##########################################
#############################################################


# SAVE --------------------------------------------------------------------

  saveRDS(
    algae_traits,
    file = file.path(output, "flowcam", "algae_traits_filtered.rds")
  )


# Finalize ----------------------------------------------------------------

  unlink(processing)
  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
