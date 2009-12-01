\name{days.agg}
\alias{days.agg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ This function is designed to aggregate time series data into daily blocks.
%%  ~~function to do ... ~~
}
\description{ This function will aggregate a time series formatted data set into days, or a multiple of days. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
days.agg(data, process, multiple = NULL, na.rm = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ The time series formatted dataset
%%     ~~Describe \code{data} here~~
}
  \item{process}{ The statistical method (mean,max,min,sum,etc)
%%     ~~Describe \code{process} here~~
}
  \item{multiple}{ The number of days to agregate to, the defualt is 1 day.
%%     ~~Describe \code{multiple} here~~
}
  \item{na.rm}{ If the dataset has NA's and how they should be treated. Default is F.
%%     ~~Describe \code{na.rm} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ The function returns a dataset with two columns, date (as.POSIXlt format) and data.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ Jason Lessels. The University of Sydney.
%%  ~~who you are~~
}
\note{ This package has had very little testing, please use with caution. Any problems/questions can be directed to Jason Lessels (j.lessels@usyd.edu.au).
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~timeseries }
\keyword{ ~aggregation }% __ONLY ONE__ keyword per line
