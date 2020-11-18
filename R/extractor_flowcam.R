#' Preprocessor flowcam data
#'
#' extract data from all \code{*_classes_*_data.csv} files
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>% %<>%
#' @importFrom readr read_csv cols col_double col_integer col_character col_date col_time col_datetime
#' @importFrom	stats sd
#'
#' @export
extractor_flowcam <- function( input, output ) {
  message("\n########################################################\n")
  message("\nExtracting flowcam...\n")

# Based on flowcam_classification_to_final_data.R ----------------------------------------
  # David Inauen, 19.06.2017

# Get csv file names ------------------------------------------------------

  flowcam_path <- file.path( input, "flowcam" )
  fcns <- list.dirs(
    path = flowcam_path,
    recursive = FALSE,
    full.names = FALSE
  )

  if (length(fcns) == 0) {
    message("nothing to extract\n")
    message("\n########################################################\n")
    return(invisible(FALSE))
  }

# read in all data frames as list, way faster than read.csv ---------------------------------

  classes <- lapply(
    file.path(flowcam_path, fcns, paste0(fcns, ".csv")),
    readr::read_csv,
    col_types =
      cols(
        .default = col_double(),
        `Particle ID` = col_integer(),
        Class = col_character(),
        `Calibration Image` = col_integer(),
        Camera = col_integer(),
        `Capture X` = col_integer(),
        `Capture Y` = col_integer(),
        Date = col_date(format = ""),
        `Image File` = col_character(),
        `Image Height` = col_integer(),
        `Image Width` = col_integer(),
        `Image X` = col_integer(),
        `Image Y` = col_integer(),
        `Particles Per Chain` = col_integer(),
        `Source Image` = col_integer(),
        Time = col_time(format = ""),
        Timestamp = col_datetime(format = "")
      )
  ) %>%
    # combine into one large tibble
    dplyr::bind_rows(.) %>%
    # add jar ID's to df
    dplyr::mutate(
      ID = strsplit(`Image File`, "_") %>%
        sapply("[", 4) %>%
        as.numeric %>%
        sprintf("B%02d", .)
    ) %>%
    # Define Groupings
    # dplyr::group_by(Class, ID, Date, treat, incubator) %>%
    dplyr::group_by(
      Class,
      ID,
      Date
    )

  # #### REMOVE / ADD COLOUMNS AND COMPRESS DF
  #
  # classes <- subset(df_temp, select = -c(Ch1 Area, Ch1 Peak, Ch1 Width, Ch2 Area, Ch2 Peak, Ch2 Width, Scatter Area, Scatter Peak, Scatter Width, Timestamp))


  # further changes, adding proper date, treat and incubator
  # classes$Date <- format(
  #   as.Date(
  #     classes$Date,
  #     "%d-%b-%Y"
  #   ),
  #   "%Y-%m-%d"
  # )

  # labeling treatment
  # target <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09")
  # classes$treat <- ifelse(ID %in% target, "const", "fluc")
  # classes$treat <- as.factor(classes$treat)
  #
  # # labeling incubators
  # inc_182 <- c("B01", "B02", "B03")
  # inc_181 <- c("B04", "B05", "B06")
  # inc_180 <- c("B07", "B08", "B09")
  # inc_179 <- c("B10", "B11", "B12")
  # inc_178 <- c("B13", "B14", "B15")
  # inc_177 <- c("B16", "B17", "B18")
  # classes$incubator <- ifelse(ID %in% inc_182, "182", ifelse(ID %in% inc_181, "181", ifelse(ID %in% inc_180, "180", ifelse(ID %in% inc_179, "179", ifelse(ID %in% inc_178, "178", "177")))))
  # classes$incubator <- as.factor(classes$incubator)
  #
  # # dropping out further unenecessary coloumns
  # classes <- subset(classes, select = -c(Date, Time, Camera, Calibration Image, Filter Score, Date, Particles Per Chain))

  # summarise classes
  classes_aggregated <- classes %>%
    dplyr::  summarise(
      count = length(`Particle ID`),
      mean.area.abd = mean(`Area (ABD)`),
      sd.area.abd = stats::sd(`Area (ABD)`),
      mean.area.filled = mean(`Area (Filled)`),
      sd.area.filled = stats::sd(`Area (Filled)`),
      mean.aspect.ratio = mean(`Aspect Ratio`),
      sd.aspect.ratio = stats::sd(`Aspect Ratio`),
      mean.blue = mean(`Average Blue`),
      sd.blue = stats::sd(`Average Blue`),
      mean.green = mean(`Average Green`),
      sd.green = stats::sd(`Average Green`),
      mean.red = mean(`Average Red`),
      sd.red = stats::sd(`Average Red`),
      mean.circle.fit = mean(`Circle Fit`),
      sd.circle.fit = stats::sd(`Circle Fit`),
      mean.circularity = mean(`Circularity`),
      sd.circularity = stats::sd(`Circularity`),
      mean.circularity.hu = mean(`Circularity (Hu)`),
      sd.circularity.hu = stats::sd(`Circularity (Hu)`),
      mean.compactness = mean(Compactness),
      sd.compactness = stats::sd(Compactness),
      mean.convex.perimeter = mean(`Convex Perimeter`),
      sd.convex.perimeter = stats::sd(`Convex Perimeter`),
      mean.convexity = mean(Convexity),
      sd.convexity = stats::sd(Convexity),
      mean.diameter.asd = mean(`Diameter (ABD)`),
      sd.diameter.asd = stats::sd(`Diameter (ABD)`),
      mean.diameter.esd = mean(`Diameter (ESD)`),
      sd.diameter.esd = stats::sd(`Diameter (ESD)`),
      mean.edge.gradient = mean(`Edge Gradient`),
      sd.edge.gradietn = stats::sd(`Edge Gradient`),
      mean.elongation = mean(Elongation),
      sd.elongation = stats::sd(Elongation),
      mean.feret.angle.max = mean(`Feret Angle Max`),
      sd.feret.angle.max = stats::sd(`Feret Angle Max`),
      mean.feret.angle.min = mean(`Feret Angle Min`),
      sd.feret.angle.min = stats::sd(`Feret Angle Min`),
      mean.fiber.curl = mean(`Fiber Curl`),
      sd.fiber.curl = stats::sd(`Fiber Curl`),
      mean.fiber.straightness = mean(`Fiber Straightness`),
      sd.fiber.straightness = stats::sd(`Fiber Straightness`),
      mean.geodesic.aspect.ratio = mean(`Geodesic Aspect Ratio`),
      sd.geodensic.aspect.ratio = stats::sd(`Geodesic Aspect Ratio`),
      mean.geodesic.length = mean(`Geodesic Length`),
      sd.geodesic.length = stats::sd(`Geodesic Length`),
      mean.geodesic.thickness = mean(`Geodesic Thickness`),
      sd.geodesic.thickness = stats::sd(`Geodesic Thickness`),
      mean.intensity = mean(Intensity),
      sd.intensity = stats::sd(Intensity),
      mean.length = mean(Length),
      sd.length = stats::sd(Length),
      mean.perimeter = mean(Perimeter),
      sd.perimeter = stats::sd(Perimeter),
      mean.ratio.blue.green = mean(`Ratio Blue/Green`),
      sd.ratio.blue.green = stats::sd(`Ratio Blue/Green`),
      mean.ratio.red.blue = mean(`Ratio Red/Blue`),
      sd.ratio.red.blue = stats::sd(`Ratio Red/Blue`),
      mean.ratio.red.green = mean(`Ratio Red/Green`),
      sd.ratio.red.green = stats::sd(`Ratio Red/Green`),
      mean.roughness = mean(Roughness),
      sd.roughness = mean(Roughness),
      mean.sigma.intensity = mean(`Sigma Intensity`),
      sd.sigma.intensity = stats::sd(`Sigma Intensity`),
      mean.sum.intensity = mean(`Sum Intensity`),
      sd.sum.intensity = stats::sd(`Sum Intensity`),
      mean.symmetry = mean(Symmetry),
      sd.symmetry = stats::sd(Symmetry),
      mean.transparency = mean(Transparency),
      sd.transparency = stats::sd(Transparency),
      mean.volume.abd = mean(`Volume (ABD)`),
      sd.volume.abd = stats::sd(`Volume (ABD)`),
      mean.volume.esd = mean(`Volume (ESD)`),
      sd.volume.esd = stats::sd(`Volume (ESD)`),
      mean.width = mean(Width),
      sd.width = stats::sd(Width)
    )

# SAVE --------------------------------------------------------------------

  add_path <- file.path( output, "flowcam" )
  dir.create( add_path, recursive = TRUE, showWarnings = FALSE )
  #
  names(classes) <- tolower(names(classes))
  saveRDS(
    object = classes,
    file = file.path(add_path, "flowcam_raw.rds")
  )
  #
  names(classes_aggregated) <- tolower(names(classes_aggregated))
  saveRDS(
    object = classes_aggregated,
    file = file.path(add_path, "flowcam_aggregated.rds")
  )

# Finalize ----------------------------------------------------------------

  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
