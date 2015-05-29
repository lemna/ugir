\name{select.all.first}
\alias{select.all.first}
\title{
Select all records for a patient occuring on the first date in a patient registry
}
\description{This function orders the dataframe on patient and date, then selects all records for each patient that occur on the first day in the dataframe.}
\usage{select.all.first(x, lpnr = 'lpnr', date = 'diadat')}
\arguments{
  \item{x}{dataframe with at least a field that uniquely identifies each patient and a date}
  \item{lpnr}{name of variable in \code{x} that uniquely identifies each patient. Defaults to \code{lpnr}}
  \item{date}{name of date variable, used to order \code{x}. Defaults to \code{diadat}}}
\details{
The patient registries typically contain a variable called \code{lpnr} and \code{diadat}, so these are the default values used. It is advisable, but not necessary, to use one of the date datatypes for \code{date}.
}
\value{
A dataframe ordered on \code{lpnr} and \code{date}, containing all records for each subject occuring on the earliest date.
}
\author{Peter Konings}
\keyword{ manip }
