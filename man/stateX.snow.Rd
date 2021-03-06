% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stateX.snow.R
\name{stateX.snow}
\alias{stateX.snow}
\title{Update the information related to snow}
\usage{
stateX.snow(htempX, hprecipX, scaobX, snowX, modelSnow, modelPrecipLZ)
}
\arguments{
\item{htempX}{temperature for each elevation zone}

\item{hprecipX}{precipitation for each elevation zone}

\item{scaobX}{snow coverage observation}

\item{snowX}{snow}

\item{modelSnow}{list(nbLevelZone,unitsnow,n0,Ws,TS,CX,CFR,CGLAC,gca,UP)}

\item{modelPrecipLZ}{list(nbLevelZone,Plr,hfelt,midmetp)}
}
\description{
Update the information related to snow
}
\examples{
\dontrun{
stateX.snow()
}
}
\keyword{snow}

