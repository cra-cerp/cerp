#' This script creates the not in operator
#'
#' @description
#' This operator is the negated version of the %in% operator.
#'
#' @param x vector of values.
#' @param y vector of values.
#'
#' @examples
#' values_vec <- 1:4
#' compare_vec <- sample(1:10, size = 2, replace = FALSE)
#'
#' @export
"%notin%" <- function(x, y){

### quick check on required parameters
stopifnot("\nThe x variable you supplied is not of type vector." = is.vector(x),
          "\nThe x variable you supplied is not of type vector." = is.vector(y))

### search and return
match(x, y, nomatch = 0) == 0

}
