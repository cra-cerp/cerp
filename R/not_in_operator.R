#' Negation of in
#'
#' @description
#' This operator is the negated version of the %in% operator.
#'
#' @param x input vector to compare
#' @param table values to compare against
#'
#' @examples
#'
#' a <- c(1,2,3)
#' 30 %notin% a
#'
#' @export
"%notin%" <- function(x, table){

match(x, table, nomatch = 0L) == 0L

}
