".First.lib" <- function(lib, pkg)
{
  library.dynam("TSAgg", package = pkg, lib.loc = lib)
  messages <- as.logical(ifelse(is.null(getOption("TSAgg.messages")),
                                TRUE, getOption("TSAgg.messages")))
  if(messages){
    cat("\n")
    cat("-------------------------------------------------------------\n")
    pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION",
                                package="TSAgg"),
                              fields=c("Title","Version","Date")))
    cat(pkg.info["Title"])
    cat("\n")
    cat("This package is designed to make time series aggregation easy.")
    cat(paste("TSAgg version ", pkg.info["Version"],
              " (built on ", pkg.info["Date"], ") is now loaded\n", sep=""))
cat(paste("For more infomation or to report issues please contact",pkg.info["Maintainer"]))
    cat("-------------------------------------------------------------\n")
    cat("\n")
  }
  return(invisible(0))
}
