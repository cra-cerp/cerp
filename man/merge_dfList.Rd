% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_dfList.R
\name{merge_dfList}
\alias{merge_dfList}
\title{Function to perform a general merge list of data frames}
\usage{
merge_dfList(dfLists)
}
\arguments{
\item{dfLists}{list of data frames or tibbles.}
}
\description{
This is a generalized version of merge_dataframes_lists as key ids are chosen from data sets
that are merged. NOTE: by default no sorting takes place and the key data frame is the first
data frame that appears in the list
}
\examples{
# Example
dataFrame1 <- data.frame(CERPID = sample(paste0("000", 1:9), size = 9, replace = FALSE),
student = sample(1:4, size = 9, replace = TRUE))
dataFrame2 <- data.frame(gender = sample(c("woman","man", "non-binary"), size = 9, replace = TRUE),
CERPID = sample(paste0("000", 1:9), size = 9, replace = FALSE))
merge_dfList(list(dataFrame1, dataFrame2))

}
