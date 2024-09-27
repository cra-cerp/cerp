#' Global matching parameters function
#'
#' @description
#' This function is primarily used to search for variables when generating data for analysis.
#'
#' @param variableName A variable name.
#' @param \dots Additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{groupFlag.}: An optional parameter. that is a character vector containing the group
#'  or time flag pattern to identify each variable stem. The default is "_w". This default can be
#'  overridden by specifying another group or time flag pattern.
#'  }
#'
#' @author Ama Nyame-Mensah
#'
#' @export
matchingParams <- function(variableName, ...) {

### quick check for variableName
stopifnot("\nThe variable names argument you supplied is not of type vector." = is.vector(variableName))

### proceed otherwise
## extract other specified arguments (these are optional)
dots <- list(...)
# set groupFlag
groupFlag <- if (!is.null(dots[["groupFlag"]])) dots[["groupFlag"]] else "_w"

## return variable names to match
c(paste0("^", variableName, "$"),paste0("^", variableName, groupFlag))

}
