\name{yearsAgg}
\alias{yearsAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Yearly aggregation function
%%  ~~function to do ... ~~
}
\description{ This function allows for the aggregation of non-continuos time series data into years. It allows for the multiple years to be specified. The aggregation method is also allowed to be set by the user. It also allows for the aggregation block to start at first observation or from the start of the year.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
yearsAgg(data, process, multiple = NULL, na.rm = F, from.first.obs = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A \code{\link{timeSeries}} formatted dataset.
%%     ~~Describe \code{data} here~~
}
  \item{process}{ An aggregation method. See \code{\link{aggregation}}.
%%     ~~Describe \code{process} here~~
}
  \item{multiple}{ An integer specifying the aggregation block.
%%     ~~Describe \code{multiple} here~~
}
  \item{na.rm}{ ...
%%     ~~Describe \code{na.rm} here~~
}
  \item{from.first.obs}{ Wether the years start from the first observation or from the beginning of the year.
%%     ~~Describe \code{from.first.obs} here~~
}
}
\details{ This function is based on the \code{\link{aggregation}} method. It is designed to simplify the process of aggregating non-continuos data.
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

\seealso{ \code{\link{aggregation}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
