\name{create.flowchart}
\alias{create.flowchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an empty flowchart dataframe
}
\description{
This function creates an empty dataframe with appropriate fields for use as the basis of a flowchart plot. Every row describes one node of a directed graph.
}
\usage{
create.flowchart()
}
\details{
No arguments are allowed.
}
\value{
An empty dataframe with the following variables:
\item{label }{character vector containing node labels, used to refer to a node in the \code{from} variable}
\item{title }{character vector containing extended node titles, used for display}
\item{display.r }{logical; when set to \code{TRUE}, the number of records is displayed in the node}
\item{display.n }{logical; when set to \code{TRUE}, the number of patients is displayed in the node}
\item{r }{integer: number of records in a node}
\item{n }{integer: number of patients in a node}
\item{from}{character vector, refers to the \code{label} of the parent node}
}
\author{
Peter Konings
}
\seealso{
\code{\link{create.diagram}} to create a valid diagram statement from this type of dataframe. \code{\link{add.node}} adds one or two nodes as row(s) to this type of dataframe.
}
\examples{
testchart <- create.flowchart()
testchart
}
\keyword{manip}
