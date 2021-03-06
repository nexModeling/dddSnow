% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init.snow.R
\name{init.snow}
\alias{init.snow}
\title{Initialize information related to snow}
\usage{
init.snow(method = NULL, path = NULL, isoil = NULL, gisoil = NULL,
  spd = NULL, wcd = NULL, sca = NULL, nsno = NULL, alfa = NULL,
  ny = NULL, snowfree = NULL)
}
\arguments{
\item{method}{method for the initialization, "load", "source", "manual"}

\item{path}{directory where to get the files, in used when method is "load" or "source"}

\item{isoil}{precipitation and/or snowmelt from the elevation zones, vector(level zone)}

\item{gisoil}{glaciermelt from the elevation zones, vector(level zone)}

\item{spd}{Snow Water Equivalent in BV (NEED MORE EXPLANATION)}

\item{wcd}{Free Water in BV (NEED MORE EXPLANATION)}

\item{sca}{snow coverage (NEED MORE EXPLANATION)}

\item{nsno}{number of event (NEED MORE EXPLANATION)}

\item{alfa}{alfa parameter i nedbor gamma fordeling (NEED MORE EXPLANATION)}

\item{ny}{ny parameter i nedbor gamma fordeling (NEED MORE EXPLANATION)}

\item{snowfree}{percentage of snowfree area}
}
\description{
Initialize the information related to snow
}
\examples{
\dontrun{
init.snow()
}
}
\keyword{snow}

