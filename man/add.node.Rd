\name{add.node}
\alias{add.node}
\title{
Add a node to a flowchart.
}
\description{
This function adds one or two rows to a flowchart dataframe. Used to describe a dataframe after manipulation.
}
\usage{
add.node(dataframe, flowchart, label, title, from = NA,
  display.r = TRUE, display.n = TRUE)
}
\arguments{
  \item{dataframe}{
The dataframe to be described. It should at least contain a unique patient identifier called \code{lpnr}
}
  \item{flowchart}{
A flowchart dataframe
}
  \item{label}{
character, with the node label, used to refer to this node in the \code{from} variable
}
  \item{title}{
character; extended node title, used for display
}
  \item{id}{character, name of the patient id. Defaults to lpnr}
  \item{from}{
character, refers to the \code{label} of the parent node. Set to NA when the node is used as top node; use a character vector to describe merging of two or more parent nodes
}
  \item{display.r}{
logical; when set to \code{TRUE}, the number of records is displayed in the node
}
  \item{display.n}{
logical; when set to \code{TRUE}, the number of patients is displayed in the node
}
}

\value{
The flowchart dataframe with one or more rows added.
}

\author{
Peter Konings
}
\seealso{
\code{\link{create.diagram}} to create a valid diagram statement from this type of dataframe. \code{\link{create.flowchart}} creates an empty flowchart dataframe for use by this function. 
}
\examples{
testchart <- create.flowchart()

testdf <- data.frame(lpnr = c(1,1,1,1:10), blah = rep('blah', 13))
testchart <- add.node(testdf, testchart,  'start', 'original dataset')
testchart
}

\keyword{ manip }
