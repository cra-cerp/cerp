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
stopifnot("\nThe x variable you supplied is not of type vector or list." =
            class(x) %in% c("list","character", "numeric", "integer"))

### otherwise, proceed
## extract other specified arguments (these are optional)
dots <- list(...)
# set addDelim
addDelim <- if (!is.null(dots[["addDelim"]])) dots[["addDelim"]] else ","

## return vector of values
toReturn <- suppressWarnings(paste(unlist(strsplit(as.character(x),split = addDelim))))
trimws(toReturn)

}

