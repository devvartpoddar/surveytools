#' On Load
#'
#' Code to set up options for onload
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.surveytools <- list(
    surveytools.force_print = FALSE
  )
  toset <- !(names(op.surveytools) %in% names(op))
  if(any(toset)) options(op.surveytools[toset])

  invisible()
}
