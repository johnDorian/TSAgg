\name{timeSeries2zoo}
\alias{timeSeries2zoo}
\title{
Convert timeSeries to zoo
}
\description{
Convert timeSeries object to zoo.
}
\usage{
timeSeries2zoo(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
timeSeries object.
}
  \item{\dots}{
currently not used.
}
}
\value{
A zoo object.
}
\author{
G. Grothendieck
}
\examples{
\dontrun{
library(zoo)
data(foo)
fooh <- head(foo)
foo.ts <- timeSeries(fooh[,1],"%d/%m/%Y   %H:%M",fooh[,3])
foo.ts
timeSeries2zoo(foo.ts)
}
}
