\name{monthsAgg}
\alias{monthsAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Monthly aggregation function

}
\description{This function allows for the aggregation of non-continuos time series data into months. It allows for the multiple months to be specified. The aggregation method is also allowed to be set by the user.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
monthsAgg(data, process, multiple = NULL, na.rm = FALSE)
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
  \item{na.rm}{ Default is na.rm=FALSE
%%     ~~Describe \code{na.rm} here~~
}
}
\details{ This function is based on the \code{aggregate} method. It is designed to simplify the process of aggregating non-continuos data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ Returns a data.frame with dates formatted as \code{as.POSIXlt} and aggregated data.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{ Jason Lessels <jason.lessels@sydney.edu.au>
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{  \code{aggregate}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Load the data
data(foo)
##Format the data using the timeSeries function.
foo.ts <- timeSeries(foo[,1],"\%d/\%m/\%Y   \%H:\%M",foo[,3])
##Aggregate the data into 6 month blocks using mins
min.month <- monthsAgg(foo.ts,min,6)
##Aggregate the data into 6 days blocks using max
max.month <- monthsAgg(foo.ts,max,6)
##Aggregate the data into 6 days blocks using max
mean.month <- monthsAgg(foo.ts,mean,6)
#Create a plot of the results
ymin=min(min.month$data);ymax=max(max.month$data)
plot(mean.month,ylim=c(ymin,ymax))
lines(min.month,type="l",lty=3,col="red")
lines(max.month,type="l",lty=3,col="blue")

}
