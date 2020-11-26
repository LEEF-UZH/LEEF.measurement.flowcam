#' Template function to assign value to parameter in the package wide cache
#'
#' assign the function to a new cvariable and the name of the function woll be used for the parameter name. e.g:
#' \itemize{
#'    \item{fps <- par_template}
#' }
#' @param value if missing, the value of the parameter will be returned, \code{NULL} if the parameter does not exist; if specified, the parameter will be set to the value
#'
#' @return the (new) value of the argument
#'
#' @importFrom utils tail
#'
#' @export
#'
#' @examples
par_template <- function(value) {
  parName <- match.call()[[1]]
  parName <- as.character(parName)
  parName <- utils::tail(parName, 1)
  parName <- gsub("par_", "", parName)
  if ( missing(value) ) {
    if (!exists(parName, envir = .FLOWCAM_CACHE, inherits = FALSE)) {
      stop("Parameter '", parName, "' not set!\n", "Set by using '", parName, "(value)' before usage!")
    }
  } else {
    assign(parName, value, envir = .FLOWCAM_CACHE)
  }
  result <- base::get(parName, envir = .FLOWCAM_CACHE, inherits = FALSE)
  return( result )
}

#' Save parameter into \code{.yaml} file
#'
#' @param file name of parameter file
#'
#' @importFrom yaml write_yaml
#' @return invisibly \code{TRUE}
#' @export
#'
#' @examples
save_parameter <- function(file = "parameter.yaml") {
  yaml::write_yaml(
    x = as.list(.FLOWCAM_CACHE),
    file = file
  )
  invisible(TRUE)
}

#' Load parameter from \code{file}
#'
#' @param file name of parameter file
#'
#' @return invisibly TRUE
#' @importFrom yaml read_yaml
#' @export
#'
#' @examples
load_parameter <- function(file = "parameter.yaml") {
  p <- yaml::read_yaml( file )
  list2env(p, envir = .FLOWCAM_CACHE)
  invisible( TRUE )
}


#' Print the flowcam parameter
#'
#' @param echo if the results should be shown on the screen or only the cache returned as a list.
#' @param print_as_yaml Print in yaml formated text; \code{~} stands for NULL
#'
#' @return invisible returns list of parameter for further processing
#'
#' @importFrom yaml as.yaml
#' @export
#'
#' @examples
print_parameter <- function(
  print_as_yaml = TRUE,
  echo = TRUE
) {
  result <- as.list(.FLOWCAM_CACHE)
  if (echo) {
    if (print_as_yaml) {
      cat(yaml::as.yaml(result))
    } else {
      print(result)
    }
  }
  return( invisible(result) )
}
