#' Extract vector of values
#'
#' @param x A character vector
#' @param \dots Additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{addDelim}: An optional parameter you can specify when retaining more than one element.
#'  The default value is ",".
#'  }
#'
#' @returns A character vector.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' vulist("1,2,3")
#'
#' @export
vulist <- function(x, ...){

### quick check for vector
stopifnot("\nThe x variable you supplied is not of type vector or list." = sum(any(is.vector(x), is.list(x))) > 0)

### Otherwise, proceed
## extract other specified arguments (these are optional)
dots_list <- list(...)
if(length(dots_list) != 0){
  dots <- unlist(dots_list)
  addDelim <- dots[grepl("adddelim", tolower(names(dots)))]
} else{
  dots <- NULL
}

## set these inputs to null/pre-determined string
addDelim <- if(is.null(dots) | !any(grepl("adddelim", tolower(names(dots))))){","} else{escape_punct(addDelim)}

## return vector of values
toReturn <- suppressWarnings(paste(unlist(strsplit(as.character(x),split = addDelim))))
trimws(toReturn)

}

