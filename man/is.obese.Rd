\name{is.obese}
\alias{is.obese}
\title{
Check whether a patient is obese
}
\description{
This function checks whether a patient is obese, using the appropriate ICD codes.
}
\usage{
is.obese(dataframe, icd, pattern = "dia")
}
\arguments{
  \item{dataframe}{a dataframe containing at least one diagnosis variable
}
  \item{icd}{
Vector with icd period for each record
}
  \item{pattern}{
pattern used to determine which variables are diagnosis variables
}
}

\value{
A logical vector, TRUE if a record contains at least one obesity code, FALSE otherwise. Set to NA if ICD codes fall outside ICD 7 - ICD 10.
}

\author{
Peter Konings
}



\seealso{
icdversion
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dataframe, icd, pattern = "dia") 
{
    stopifnot(all(icd \%in\% paste("icd", 7:10, sep = "")))
    temp.df <- subset(dataframe, select = grepl(x = names(dataframe), 
        pattern = pattern))
    icd7 <- do.call(cbind, lapply(temp.df[icd == "icd7", ], grepl, 
        pattern = "^287"))
    icd8 <- do.call(cbind, lapply(temp.df[icd == "icd8", ], grepl, 
        pattern = "^277"))
    icd9 <- do.call(cbind, lapply(temp.df[icd == "icd9", ], grepl, 
        pattern = "^278A"))
    icd10 <- do.call(cbind, lapply(temp.df[icd == "icd10", ], 
        grepl, pattern = "^E66"))
    icd7 <- apply(icd7, 1, any)
    icd8 <- apply(icd8, 1, any)
    icd9 <- apply(icd9, 1, any)
    icd10 <- apply(icd10, 1, any)
    obese <- rep(NA, times = nrow(temp.df))
    obese[icd == "icd7"] <- icd7
    obese[icd == "icd8"] <- icd8
    obese[icd == "icd9"] <- icd9
    obese[icd == "icd10"] <- icd10
    return(obese)
  }
}

\keyword{manip}
