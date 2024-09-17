#' Function to escape punctuation for regex search .
#'
#' @param x a character vector.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' stringSearch <- "All punctuations must be escaped. Will this function escape them? Yes."
#' escape_punct(stringSearch)
#'
#' @export
escape_punct <- function(x) {

### quick check on required parameters
stopifnot("\nThe x variable you supplied is not of type vector." = is.vector(x))

### otherwise, proceed to escape + return elements
gsub("([][{}()+*^${|\\\\?.])", "\\\\\\1", unlist(x))

}
