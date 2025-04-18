% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform-head.R
\name{new_head_block}
\alias{new_head_block}
\title{Subset block constructor}
\usage{
new_head_block(n = 6L, direction = c("head", "tail"), ...)
}
\arguments{
\item{n}{Number of rows}

\item{direction}{Either "head" or "tail"}

\item{...}{Forwarded to \code{\link[=new_block]{new_block()}}}
}
\description{
This block allows to perform row and column subsetting on \code{data.frame}
objects (see \code{\link[base:subset]{base::subset()}}).
}
