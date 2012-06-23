\name{daysAgg}
\alias{daysAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Daily aggregation function}
\description{ This function allows for the aggregation of non-continuos time series data into days. It allows for the multiple days to be specified. The aggregation method is also allowed to be set by the user.}
\usage{
daysAgg(data, process, multiple = NULL, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A \code{\link{timeSeries}} formatted dataset.
%%     ~~Describe \code{data} here~~
}
  \item{process}{ An aggregation method. See \code{aggregate}.
%%     ~~Describe \code{process} here~~
}
  \item{multiple}{ An integer specifying the aggregation block. Default is multiple=NULL.
%%     ~~Describe \code{multiple} here~~
}
  \item{na.rm}{ Default is na.rm=FALSE.}
}
\details{ This function is based on the \code{aggregate} method. It is designed to simplify the process of aggregating non-continuos data.}
\value{ Returns a data.frame with dates formatted as \code{as.Date} and aggregated data.}
\references{ Returns a data.frame with dates formatted as \code{as.Date} and aggregated data.}
\author{ Jason Lessels <jason.lessels@sydney.edu.au>}

\examples{
#Load the data:
data(foo)
#Format the data using the timeSeries function.
foo.ts<-timeSeries(foo[,1],  "\%d/\%m/\%Y  \%H:\%M",foo[,3])
#Aggregate the data into days using mean:
foo.1day<-daysAgg(foo.ts,mean)
#Aggregate the data into weeks, using 7 days and mean:
foo.week<-daysAgg(foo.ts,mean,7)
}
