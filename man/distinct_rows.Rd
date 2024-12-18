% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distinct_rows.R
\name{distinct_rows}
\alias{distinct_rows}
\title{A function to retain/keep unique rows}
\usage{
distinct_rows(df, column, retain, keep_allVars, ...)
}
\arguments{
\item{df}{A tibble or data frame.}

\item{column}{A key column.}

\item{retain}{Which record to retain (i.e., first/last). Parameter governs whether the first or last
instance of each duplicate is retained. NOTE: if you need to keep the first or last instance of a
duplicate based on some other variable (like a date), first sort the data set by that column and then
use the function.}

\item{keep_allVars}{TRUE/FALSE; if TRUE all columns are retained, else only the key column is retained.}

\item{\dots}{additional parameters to pass to function. These include:
\itemize{
 \item \code{columnN}: An optional parameter where "N" corresponds to a number.
 Add additional column variables followed by a number (e.g., column2, column3)
 to consider additional key columns when retaining distinct/unique rows.
 }}
}
\value{
A data frame.
}
\description{
This function is similar to dplyr's distinct() function and can be used to
retain distinct/unique rows.
}
\examples{
# Example with one key column (want to keep earliest date)
distinct_example <- data.frame(id = c("id_3","id_1","id_2","id_2","id_1","id_3"),
date = as.Date(c("2024-05-01","2024-08-01","2024-02-14","2024-05-01","2024-05-01","2024-01-01")),
participantCode = c("NSF REU", "CAHSI", "DREU","DREU", "CAHSI", "NSF REU"))
distinct_example <- distinct_example[order(distinct_example$id, distinct_example$date),]
distinct_rows(df = distinct_example, column = id, retain = first, keep_allVars = TRUE)

# Example with multiple key columns and sorting
# Here the latest date record by wave per id is retained
distinct_example2 <- data.frame(id = c("cerpid_001","cerpid_002","cerpid_003","cerpid_003",
"cerpid_001","cerpid_001","cerpid_002","cerpid_003"),wave_flag = c("pre-survey", "pre-survey",
"post-survey","post-survey", "post-survey", "post-survey","post-survey","pre-survey"),
date = as.Date(c("2024-05-25","2024-05-01","2024-08-01","2024-03-20","2024-06-08","2024-06-03",
"2024-08-12","2024-02-14")))
distinct_example2 <- distinct_example2[order(distinct_example2$id, distinct_example2$date),]
distinct_example2R <- distinct_rows(df = distinct_example2, column = id, retain = last,
keep_allVars = TRUE, column2 = wave_flag)

}
\author{
Ama Nyame-Mensah
}
