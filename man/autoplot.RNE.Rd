% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.RNE.R
\name{fortify.RNE}
\alias{fortify.RNE}
\alias{autoplot.RNE}
\title{autoplot for palaeosig rne}
\usage{
\method{fortify}{RNE}(object, which = 1, unit = "")

\method{autoplot}{RNE}(object, which = 1, ylim, unit = "km")
}
\arguments{
\item{object}{RNE object from palaeosig}

\item{which}{which reconstruction}

\item{unit}{character. Neighbourhood units}

\item{ylim}{numeric vector length 2. y limits}
}
\description{
Plots of r2 against fraction of sites deleted randomly, by neighbourhood or by environmental similarity.
}
\examples{
  require(fields)
  require(palaeoSig)
  require(rioja)
  require(ggplot2)
  data(arctic.env)
  data(arctic.pollen)

  #using just the first 100 sites so code runs in an reasonable time, but still slow
  arctic.d <- rdist.earth(arctic.env[1:100,c("Longitude","Latitude")], miles = FALSE)
  arctic.rne <- rne(y = arctic.pollen[1:100,], env = arctic.env$tjul[1:100],
    geodist = arctic.d, fun = MAT, neighbours = c(0,50,200),
    subsets = c(1,.5,.1), k = 5)
autoplot(arctic.rne)
}
