\name{NEWS}

\title{News for package \pkg{ugir}}
\section{Changes in version 0.2.4 (2015-03-05)}{
\itemize{
\item \code{add.node()} now checks if 'from' labels exist
\item \code{add.node()} has an extra \code{id} argument, to allow for files were the patient identification is not called 'lpnr'
}
}
\section{Changes in version 0.2.3 (2015-02-24)}{
\itemize{
\item bugfix in \code{create.diagram()}: no to-nodes created when only from-nodes are present
}
}
\section{Changes in version 0.2.2 (2015-02-23)}{
\itemize{
\item bugfix in \code{add.node()} and \code{create.flowchart()}
\item versioning changed to something more sensible
}
}
\section{Changes in version 0.2.1 (2015-02-16)}{
\itemize{
\item added support for unconnected nodes in \code{create.diagram()}
}
}

\section{Changes in version 0.2 (2015-02-12)}{
\itemize{
\item packages \pkg{DiagrammeR} and \pkg{lubridate} are now required and properly imported
\item function \code{create.flowchart()} to create an empty flowchart dataframe
\item function \code{add.node()} to add nodes and optionally edges to an existing flowchart dataframe
\item function \code{create.diagram()} to create a diagram statement from a flowchart dataframe for use by \code{grViz}
}
}

\section{Changes in version 0.1 (2015-01-19)}{
\itemize{
\item Initial commit, with functions \code{check.sex()}, \code{icdversion()}, \code{is.obese()}, \code{report.date.errors()} and \code{select.all.first()}
}
