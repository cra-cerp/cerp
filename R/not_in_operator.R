#' This script creates the not in operator
#'
#' @description
#' This operator is the negated version of the %in% operator.
#'
#' @param x input vector to compare
#' @param table values to compare against
#'
#' @export
`%notin%` <- function(x, table){

match(x, table, nomatch = 0L) == 0L

}



