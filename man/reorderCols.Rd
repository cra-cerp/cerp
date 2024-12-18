% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reorderCols.R
\name{reorderCols}
\alias{reorderCols}
\title{Reorder column rows in a data frame or tibble}
\usage{
reorderCols(dataSet, colReorder, newOrder)
}
\arguments{
\item{dataSet}{A tibble or data frame.}

\item{colReorder}{The name of the column to reorder.}

\item{newOrder}{A character vector containing a user-specified order for column string/values.}
}
\value{
A tibble/data frame.
}
\description{
This function allows the user to specify the order of rows in a column by supplying a vector of
values (newOrder).
}
\examples{
outOfOrder <- data.frame(values = c("zucchini", "banana", "apple", "carrot"),
letter = c("z", "b", "a","c"))
inOrder <- outOfOrder
reorderCols(dataSet = outOfOrder, colReorder = "values",
newOrder = c( "apple","carrot","banana","zucchini"))

}
\author{
Ama Nyame-Mensah
}
