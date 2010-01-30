\name{hoursAgg}
\alias{hoursAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hour aggregation function.
%%  ~~function to do ... ~~
}
\description{ This function allows for the aggregation of non-continuos time series data into hours. It allows for the multiple hours to be specified. The aggregation method is also allowed to be set by the user.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hoursAgg(data, process, multiple = NULL, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A \code{timeSeries} formatted dataset.
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
}
\details{ This function is based on the \code{\link{aggregation}} method. It is designed to simplify the process of aggregating non-continous data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ Jason Lessels
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
## The function is currently defined as
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
