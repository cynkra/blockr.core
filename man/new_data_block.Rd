% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-block.R, R/data-dataset.R, R/data-static.R
\name{new_data_block}
\alias{new_data_block}
\alias{new_dataset_block}
\alias{new_static_block}
\title{Data block constructors}
\usage{
new_data_block(server, ui, class, ctor = sys.parent(), ...)

new_dataset_block(dataset = character(), package = "datasets", ...)

new_static_block(data, ...)
}
\arguments{
\item{server}{A function returning \code{\link[shiny:moduleServer]{shiny::moduleServer()}}}

\item{ui}{A function with a single argument (\code{ns}) returning a \code{shiny.tag}}

\item{class}{Block subclass}

\item{ctor}{String-valued constructor name or function/frame number (mostly
for internal use or when defining constructors for virtual classes)}

\item{...}{Forwarded to \code{new_data_block()} and \code{\link[=new_block]{new_block()}}}

\item{dataset}{Selected dataset}

\item{package}{Name of an R package containing datasets}

\item{data}{Data (used directly as block result)}
}
\value{
All blocks constructed via \code{new_data_block()} inherit from
\code{data_block}.
}
\description{
Data blocks typically do not have data inputs and represent root nodes in
analysis graphs. Intended as initial steps in a pipeline, such blocks are
responsible for providing down-stream blocks with data.
}
\section{Dataset block}{

This data block allows to select a dataset from a package, such as
the datasets package available in most R installations as one of the
packages with "recommended" priority. The source package can be chosen at
time of block instantiation and can be set to any R package, for which then
a set of candidate datasets is computed. This includes exported objects that
inherit from \code{data.frame}.
}

\section{Static block}{

Mainly useful for testing and examples, this block simply returns the data
with which it was initialized. Serialization of static blocks is not allowed
and exported code will not be self-contained in the sense that it will not
be possible to reproduce results in a script that contains code from a
static block.
}

