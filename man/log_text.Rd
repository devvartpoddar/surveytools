% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logging.r
\name{log_text}
\alias{log_text}
\title{Log text}
\usage{
log_text(..., log = NULL, print = getOption("surveytools.force_print"))
}
\arguments{
\item{...}{text to be rendered using rmarkdown}

\item{log}{Overwrite default log that gets logged to and rendered}
}
\description{
Log text using rmarkdown. Since it uses rmarkdown to render, a whole range of markdown as well as html are supported
}
