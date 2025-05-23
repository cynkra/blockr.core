% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/registry.R
\name{register_block}
\alias{register_block}
\alias{list_blocks}
\alias{unregister_blocks}
\alias{register_blocks}
\alias{available_blocks}
\alias{create_block}
\title{Block registry}
\usage{
register_block(
  ctor,
  name,
  description,
  classes = NULL,
  uid = NULL,
  category = "uncategorized",
  package = NULL,
  overwrite = FALSE
)

list_blocks()

unregister_blocks(uid = list_blocks())

register_blocks(...)

available_blocks()

create_block(id, ...)
}
\arguments{
\item{ctor}{Block constructor}

\item{name, description}{Metadata describing the block}

\item{classes}{Block classes}

\item{uid}{Unique ID for a registry entry}

\item{category}{Useful to sort blocks by topics. If not specified,
blocks are uncategorized.}

\item{package}{Package where constructor is defined (or \code{NULL})}

\item{overwrite}{Overwrite existing entry}

\item{...}{Forwarded to \code{register_block()}}

\item{id}{Block ID as reported by \code{list_blocks()}}
}
\value{
\code{register_block()} and \code{register_blocks()} are invoked for their side
effects and return \code{registry_entry} object(s) invisibly, while
\code{unregister_blocks()} returns \code{NULL} (invisibly). Listing via \code{list_blocks()}
returns a character vector and a list of \code{registry_entry} object(s) for
\code{available_blocks()}. Finally, \code{create_block()} returns a newly instantiated
\code{block} object.
}
\description{
Listing of blocks is available via a block registry, which associates a block
constructor with metadata in order to provide a browsable block directory.
Every constructor is identified by a unique ID (uid), which by default is
generated from the class vector (first element). If the class vector is not
provided during registration, an object is instantiated (by calling the
constructor with arguments \code{ctor} and \code{ctor_pkg} only) to derive this
information. Block constructors therefore should be callable without block-
specific arguments.
}
\details{
Due to current requirements for serialization/deserialization, we keep track
the constructor that was used for block instantiation. This works most
reliable whenever a block constructor is an exported function from a package
as this function is guaranteed to be available in a new session (give the
package is installed in an appropriate version). While it is possible to
register a block passing a "local" function as \code{ctor}, this may introduce
failure modes that are less obvious (for example when such a constructor
calls another function that is only defined within the scope of the session).
It is therefore encouraged to only rely on exported function constructors.
These can also be passed as strings and together with the value of \code{package},
the corresponding function can easily be retrieved in any session.

Blocks can be registered (i.e. added to the registry) via \code{register_block()}
with scalar-valued arguments and \code{register_blocks()}, where arguments may be
vector-valued, while de-registration (or removal) is handled via
\code{unregister_blocks()}. A listing of all available blocks can be created as
\code{list_blocks()}, which will return registry IDs and \code{available_blocks()},
which provides a set of (named) \code{registry_entry} objects. Finally, block
construction via a registry ID is available as \code{create_block()}.
}
\examples{
blks <- list_blocks()
register_block("new_dataset_block", "Test", "Registry test",
               uid = "test_block", package = "blockr.core")
new <- setdiff(list_blocks(), blks)
unregister_blocks(new)
setequal(list_blocks(), blks)

}
