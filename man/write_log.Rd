% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-logging.R
\name{write_log}
\alias{write_log}
\alias{log_fatal}
\alias{log_error}
\alias{log_warn}
\alias{log_info}
\alias{log_debug}
\alias{log_trace}
\alias{as_log_level}
\alias{cnd_logger}
\alias{cat_logger}
\title{Logging}
\usage{
write_log(..., level = "info")

log_fatal(...)

log_error(...)

log_warn(...)

log_info(...)

log_debug(...)

log_trace(...)

as_log_level(level)

cnd_logger(msg, level)

cat_logger(msg, level)
}
\arguments{
\item{...}{Concatenated as \code{paste0(..., "\\n")}}

\item{level}{Logging level (possible values are "fatal", "error", "warn",
"info", "debug" and "trace"}

\item{msg}{Message (string)}
}
\description{
Infrastructure for logging messages.
}
