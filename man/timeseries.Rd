\name{timeseries}
\Rdversion{0.1-beta}
\alias{timeseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time Series
}
\description{
This function will provide a data.frame with several columns. The first column will be
the original date column provided in the correct format. Several columns after this of each temporal element.
If data is provided this will also be included (optional).
}
\usage{
timeseries(dates, dateformat, data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dates}{
A column or vector of dates. Must be atleast three elements.
}
  \item{dateformat}{
The format of the \code{dates}. See also \code{\link{strptime}}.
}
  \item{data}{
An optional column(s) or vector. This must be the same length as the \code{dates}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{data.frame }{A data.frame containing the formatted dates and each component of the date.}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jason Lessels. PhD candiate, Faculty of Agriculture, Food and Natural Resources. The University of Sydney.
}
\note{
Still in beta, with VERY LIMITED TESTING.
}
\section{Warning }{VERY LIMITED TESTING has been conducted with this.} 

\seealso{
\code{\link{strptime}}
}
\examples{

dates<-c("01-01-1990","02-01-1990","03-01-1990","05-03-1990","01-05-1990")
data<-c(1,2,3,4,5)

timeseries(dates,"%d-%m-%Y",data)
timeseries(dates,"%d-%m-%Y")

#Data without days 
dates<-c("jan-1990","jan-1990","jan-1990","mar-1990","may-1990")

timeseries(paste(01,dates),"%d %b-%Y")

#Dates with time
dates<-c("01-01-1990 11:30","02-01-1990 12:56","03-01-1990 10:34","05-03-1990 9:01","01-05-1990 9:01")
data<-c(1,2,3,4,5)

timeseries(dates,"%d-%m-%Y %H:%M",data)

#Data with multiple data columns
dates<-c("01-01-1990","02-01-1990","03-01-1990","05-03-1990","01-05-1990")
data<-data.frame(x1=seq(1:5),x2=rnorm(5),x3=rep(1,5))

timeseries(dates,"%d-%m-%Y",data)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~timeseries }
\keyword{ ~strip }% __ONLY ONE__ keyword per line
