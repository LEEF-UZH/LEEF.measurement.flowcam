# Create cache environment ------------------------------------------------

.FLOWCAM_CACHE <- new.env(FALSE, parent = globalenv())

.onLoad <- function(lib, pkg) {
  opt <-  list(
    debug = FALSE
  )
  options(LEEF.measurement.flowcam = opt)
}

utils::globalVariables(
	c(
		".", "Area (ABD)", "Area (Filled)", "Aspect Ratio", "Average Blue",
		"Average Green", "Average Red", "Circle Fit",
		"Circularity", "Circularity (Hu)", "Class", "Compactness",
		"Convex Perimeter", "Convexity", "Date", "Diameter (ABD)",
		"Diameter (ESD)", "Edge Gradient", "Elongation", "Feret Angle Max",
		"Feret Angle Min", "Fiber Curl", "Fiber Straightness",
		"Geodesic Aspect Ratio", "Geodesic Length", "Geodesic Thickness",
		"ID", "Image File", "Intensity", "Length", "Particle ID", "Perimeter",
		"Ratio Blue/Green", "Ratio Red/Blue", "Ratio Red/Green", "Roughness",
		"Sigma Intensity", "Sum Intensity", "Symmetry", "Transparency",
		"Volume (ABD)", "Volume (ESD)", "Width"
	)
)
