\name{zoo2timeSeries}
\alias{zoo2timeSeries}
\title{
Convert zoo series to timeSeries.
}
\description{
Convert zoo series to timeSeries.
}
\usage{
zoo2timeSeries(x, ...)
}
\arguments{
  \item{x}{
A zoo series with Date or POSIXt time class and no duplicate times.
}
  \item{\dots}{
Currently not used.
}
}
\value{
A timeSeries data.frame object.
}
\author{
G. Grothendieck
}

\examples{
\dontrun{
library(zoo)
z <- zoo(1:3, as.Date(0:2))
zoo2timeSeries(z)
}
}
