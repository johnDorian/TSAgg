\name{timeSeries}
\alias{timeSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Current (temporary) method for creating the preferred data.frame structure for the TSAgg package.
}
\description{ This function is required to organise the data into the required structure for all functions of the package TSAgg.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timeSeries(dates, dateformat, data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dates}{ A vector, or data.frame of one column of dates. 
%%     ~~Describe \code{dates} here~~
}
  \item{dateformat}{ A string indicating the format of the dates (e.g. "%d/%m%y"). See also \code{\link{strptime}}
%%     ~~Describe \code{dateformat} here~~
}
  \item{data}{ A vector or data.frame of data. Must be the same length as dates column.
%%     ~~Describe \code{data} here~~
}
}
\details{ 
%%  ~~ If necessary, more details than the description above ~~
}
\value{ Returns a data.frame with dates formatted as \code{\link{as.POSIXlt}} and data.
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

\seealso{ \code{\link{strptime}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Load the dataset foo:
data(foo)
#Look at the dataset foo for the date string format:
head(foo)
foo.ts <- timeSeries(foo[,1],"\%d/\%m/\%Y   \%H:\%M",foo[,3])
#Try entering data manually into the function:
one.date <- timeSeries("08-09-30","\%y-\%m-\%d",4)
#Look at the results:
one.date

}

