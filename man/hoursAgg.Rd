\name{hoursAgg}
\alias{hoursAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hour aggregation function.

}
\description{ This function allows for the aggregation of non-continuos time series data into hours. It allows for the multiple hours to be specified. The aggregation method is also allowed to be set by the user.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hoursAgg(data, process, multiple = NULL, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A \code{\link{timeSeries}} formatted dataset.
%%     ~~Describe \code{data} here~~
}
  \item{process}{ An aggregation method. See \code{\link{aggregate}}.
%%     ~~Describe \code{process} here~~
}
  \item{multiple}{ An integer specifying the aggregation block. Default is multiple=NULL.
%%     ~~Describe \code{multiple} here~~
}
  \item{na.rm}{ Default is na.rm=FALSE
%%     ~~Describe \code{na.rm} here~~
}
}
\details{ This function is based on the \code{\link{aggregate}} method. It is designed to simplify the process of aggregating non-continuos data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ Returns a data.frame with dates formatted as \code{\link{as.POSIXlt}} and aggregated data.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ Jason Lessels <jason.lessels@sydney.edu.au>
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{aggregate}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Load the data
data(foo)
##Format the data using the timeSeries function.
foo.ts<-timeSeries(foo[,1],"\%d/\%m/\%Y   \%H:\%M",foo[,3])
##Aggregate the data into hours using mean
hoursAgg(foo.ts,mean)
##Aggregate the data into 12 hour blocks using min
hoursAgg(foo.ts,mean,12)

##Load data with incomplete observations
data(foo.na)
##Format the data using the timeSeries function.
foo.na.ts <- timeSeries(foo.na[,1],"\%d/\%m/\%Y   \%H:\%M",foo.na[,3])
##Aggregate the data into 30 hour blocks using mean
foo.na.1hr <- hoursAgg(foo.na.ts,mean,30,na.rm=T)
##Aggregate the data into hours using mean
foo.na.1hr <- hoursAgg(foo.na.ts,max,na.rm=T)	#This can cause errors. Therefore a better approach would be to subset the data.
foo.na.ts <-subset(foo.na.ts,!(is.na(foo.na.ts$data)))
##Try again without NA's
foo.na.1hr <- hoursAgg(foo.na.ts,max,na.rm=T)
}
