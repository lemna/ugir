\name{icdversion}
\alias{icdversion}
\title{
Return ICD version
}
\description{
Returns the ICD version used at a date.
}
\usage{
icdversion(datevar)
}
\arguments{
  \item{datevar}{
Vector of POSIXct date-time objects
}
}
\value{
A vector specifying which version of the ICD codes was used for each element of \code{datevar}. A factor with levels \code{icd7}, \code{icd8}, \code{icd9} and \code{icd10}.
}

\author{
Peter Konings
}
\note{
SkÃ¥ne lags one year behind other counties in the implementation of certain ICD versions. This is not yet implemented.
}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (x) 
%{
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
