\name{report.date.errors}
\alias{report.date.errors}
\title{Report common date errors
}
\description{
This function takes a vector containing dates and tries to convert it to POSIX-ct format. It reports a number of errors that commonly occur in the Swedish registries.
}
\usage{
report.date.errors(datevar)
}
\arguments{
  \item{datevar}{
A vector containing dates
}
}
\details{
\code{datevar} will usually be a \code{factor}, \code{character} or \code{numeric} after importing a dataset. First, it is converted to \code{character}, then to POSIX-ct. A list is returned with the index of common failures.
}
\value{
A list with the following components:
\item{wrong.format}{index for all elements that consist of exactly 8 characters}
\item{no.day}{index for all elements that consist of exactly 6 characters, i.e. \code{yyyymm}}
\item{no.month}{index for all elements that consists of exactly 4 characters, i.e. \code{yyyy}}
\item{empty}{index for all empty and missing elements}
\item{not.parsable}{index for all elements with errors}
\item{wrong.date}{index for elements that are not parsable, but are not empty either}
}

\author{
Peter Konings
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (datevar) 
{
    datevar.char <- as.character(datevar)
    wrong.format <- which(nchar(datevar.char) != 8)
    no.day <- which(nchar(datevar.char) == 6)
    no.month <- which(nchar(datevar.char) == 4)
    empty <- which(is.na(datevar.char) | datevar.char == "")
    datevar.date <- ymd(datevar.char)
    not.parsable <- which(is.na(datevar.date))
    wrong.date <- setdiff(not.parsable, union(empty, union(no.day, 
        no.month)))
    result <- list(wrong.format = wrong.format, no.day = no.day, 
        no.month = no.month, empty = empty, not.parsable = not.parsable, 
        wrong.date = wrong.date)
    return(result)
  }
}

\keyword{manip}
