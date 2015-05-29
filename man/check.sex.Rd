\name{check.sex}
\alias{check.sex}
\title{Check and convert the sex variable in a register to a factor}
\description{
Checks the domain of the sex variable and returns a factor with levels \code{male} and \code{female}.
}
\usage{
check.sex(sex)
}

\arguments{
  \item{sex}{
Vector indicating sex of the subject
}
}
\details{
\code{sex} is converted to \code{character}, then converted to a \code{factor}.The Swedish registers encode males as 1 and females as 2---all other values are taken to be missing values and set to \code{NA}. A warning is given if missing values are present.
}
\value{
Factor with levels \code{male} and \code{female}.
}

\author{
Peter Konings
}

\examples{
sex <- c(1,2,3,NA)
check.sex(sex)
}

\keyword{manip}
