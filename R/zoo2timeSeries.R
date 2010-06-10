
timeSeries2zoo <- function(x, ...) {
	stopifnot(require(zoo))
	zoo(x[-seq(7)], x$dates)
}

zoo2timeSeries <- function(x, ...) {
	stopifnot(require(zoo))
	stopifnot(inherits(time(x), "Date") || inherits(time(x), "POSIXt"))
	fmt <- if (inherits(time(x), "Date")) "%Y-%m-%d" else "%Y-%m-%d %H:%M:%S"
	if (length(dim(x)) < 2) {
		DF <- data.frame(coredata(x))
		names(DF) <- deparse(substitute(x))
		timeSeries(time(x), fmt, DF)
	} else timeSeries(time(x), fmt, as.data.frame(coredata(x)))
}

