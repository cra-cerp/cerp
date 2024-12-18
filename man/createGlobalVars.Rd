% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createGlobalVars.R
\name{createGlobalVars}
\alias{createGlobalVars}
\title{General function for creating pushed/global vector of values}
\usage{
createGlobalVars(df, vars, ..., groupFlag = "_w\\\\d$")
}
\arguments{
\item{df}{A tibble or data frame object.}

\item{vars}{A character vector of unique variable (name) stems.}

\item{\dots}{additional parameters to pass to function. These include:
\itemize{
 \item \code{groupFlag}: An optional parameter that is a character vector containing the group
 or time flag pattern to identify each variable stem. The default is "_w". This default can be
 overridden by specifying another group or time flag pattern. See example 2 below.
 }}

\item{groupFlag}{A group or time flag used to identify groups of variables. Default
is "_w".}
}
\value{
A character vector that is column-binded to the original supplied df. NOTE: The most recent
(i.e., LATEST) non-null, non-missing value of the LATEST group/time flag is retained.
}
\description{
This function can be used to find and retain the most recent
response for a record.
}
\examples{
# Example with default group/time flag
sexBirth_data
createGlobalVars(df = sexBirth_data, vars = "sex")

# Example with user-specified group/time flag (note the time flag pattern is "_t")
yearBorn_data
createGlobalVars(df = yearBorn_data, vars = "yearBorn", groupFlag = "_t")

}
\author{
Ama Nyame-Mensah
}
