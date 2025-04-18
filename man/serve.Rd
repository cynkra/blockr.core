% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/board-plugins.R, R/serve.R
\name{board_plugins}
\alias{board_plugins}
\alias{serve}
\alias{serve.block}
\alias{serve.board}
\title{Serve object}
\usage{
board_plugins(which = NULL)

serve(x, ...)

\method{serve}{block}(x, id = "block", ..., data = list())

\method{serve}{board}(x, id = rand_names(), plugins = board_plugins(), ...)
}
\arguments{
\item{which}{(Optional) character vectors of plugins to include}

\item{x}{Object}

\item{...}{Generic consistency}

\item{id}{Board namespace ID}

\item{data}{Data inputs}

\item{plugins}{Board plugins}
}
\description{
Start up shiny app.
}
\section{Examples in Shinylive}{
\describe{
  \item{example-1}{
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwzEGcIbgAEs1EXYKAvAqEALUqVTtEAeiMEAnhElQMAcxaltAVzoYWRIyLGSMtOliNCAJQCENwsjFAMZnyeElIycMEccAwAbnCCEAoKEHDUAPp0RJEAJpnZ2bHiapoE5RXZUBo5efklUFzscKSFohKCYABCAPIAIkG4IQ2VzbkF7Z3dvV4DAMLaLBIA6nAsNroTU9MEs63wDDZwy-1CACos8EFH2YGTWRVhljUKdc8VUCdNHN8p9xAMoPItIRIUIAB6Hd4NOiAloFUEDOgw6F4KFmJ6IhSvP6cAHVZphTh8AHNX4oLEEIJJF4hQJgAC+AF0gA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwzEGcIbgAEs1EXYKAvAqEALUqVTtEAeiMEAnhElQMAcxaltAVzoYWRIyLGSMtOliNCAJQCENwsjFAMZnyeElIycMEccAwAbnCCEAoKEHDUAPp0RJEAJpnZ2bHiapoE5RXZUBo5efklUFzscKSFohKCYABCAPIAIkG4IQ2VzbkF7Z3dvV4DAMLaLBIA6nAsNroTU9MEs63wDDZwy-1CACos8EFH2YGTWRVhljUKdc8VUCdNHN8p9xAMoPItIRIUIAB6Hd4NOiAloFUEDOgw6F4KFmJ6IhSvP6cAHVZphTh8AHNX4oLEEIJJF4hQJgAC+AF0gA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
}
}

