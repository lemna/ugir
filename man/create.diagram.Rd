\name{create.diagram}
\alias{create.diagram}

\title{
Create a diagram object.
}
\description{
This function creates a diagram object, which contains a diagram statement for use by the \code{grViz} function and vectors of from and to nodes.
}
\usage{
create.diagram(flowchart, preamble = "digraph {\nnode[shape=box, fontname = Helvetica]")
}

\arguments{
  \item{flowchart}{
A flowchart dataframe, as produced by the \code{create.flowchart} and \code{add.node} functions
}
  \item{preamble}{
preamble used by the \code{grViz} function
}
}
\details{
See the documentation for \code{grViz} for preamble options.
}
\value{
A list containing the following items:
\item{diagram }{a diagram statement for use by \code{grViz}}
\item{from.nodes }{a character vector containing from node descriptions}
\item{to.nodes }{a character vector containing to node descriptions}
}

\author{
Peter Konings
}

\seealso{
\code{\link{create.flowchart}} creates an empty flowchart dataframe for use by this function.  \code{\link{add.node}} adds one or two nodes as row(s) to this type of dataframe.
}
\examples{
# create an empty flowchart dataframe
testchart <- create.flowchart()

# create a test dataframe and add it to the flowchart
testdf <- data.frame(lpnr = c(1,1,1,1:10), blah = rep('blah', 13))
testchart <- add.node(testdf, testchart,  'start', 'original dataset')

# create a second test dataframe and add it to the flowchart
cancertest <- data.frame(lpnr = 1:8, cancer = c(rep('benign', 4), rep('bad', 4)))
testchart <- add.node(cancertest, testchart, 'cancerstart', 'cancer registry')

# remove duplicate records from the test dataframe, add it to the flowchart
testdf <- unique(testdf)
testchart <- add.node(testdf, testchart, 'unique', 'unique records', from = 'start')

# split the test dataframe, add results to the flowchart
testdf1 <- testdf[1:5,]
testdf2 <- testdf[6:10,]
testchart <- add.node(testdf1, testchart, 'first5', 'first five records',
  from = 'unique', display.r = FALSE)
testchart <- add.node(testdf2, testchart, 'last5', 'last five records',
  from = 'unique', display.r = FALSE)

# merge two dataframes, add to flowchart
testdf1 <- merge(testdf1, cancertest, by = 'lpnr', all.x = FALSE, all.y = FALSE)
testchart <- add.node(testdf1, testchart, 'benigncases', 'benign cases',
                      from = c('cancerstart', 'first5'))


testdf2 <- merge(testdf2, cancertest, by = 'lpnr', all.x = FALSE, all.y = FALSE)
testchart <- add.node(testdf2, testchart, 'badcases', 'malignant cases',
                      from = c('cancerstart', 'last5'),display.r = FALSE)

# create yet another dataframe, unconnected to anything else
unconnecteddf <- data.frame(lpnr = c(1:5), blah = rep('blah', 5))
testchart <- add.node(unconnecteddf, testchart, 'unconnected', 'unconnected dataset')


# show the final flowchart dataframe
testchart

# create a diagram statement from the flowchart dataframe and visualize it
my.diagram <- create.diagram(testchart)
from.nodes <- my.diagram$from.nodes
to.nodes <- my.diagram$to.nodes
grViz(my.diagram$diagram)
}

\keyword{ manip }
