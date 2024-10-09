#' Function to strip group/time flags
#'
#' @description
#' This is a generalized function for stripping group/time flags from variable names.
#'
#' @param x A vector to split.
#' @param \dots Additional parameters to pass to function. These include:
#'  \itemize{
#'  \item \code{groupFlag}: An optional parameter that is a character vector containing the group
#'  or time flag pattern to identify each variable stem. The default is "_w". This default can be
#'  overridden by specifying another group or time flag pattern. See example 2 below.
#'  }
#'
#' @returns A vector of values.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with default group/time flag
#' temp_dat <- data.frame(variable_name = c("sex_w1","sex_w2","sex"))
#' temp_dat$varStem <- rename_cols(x = temp_dat$variable_name)
#' temp_dat
#'
#' # Example with user-specified group/time flag
#' names(yearBorn_data)
#' rename_cols(x = names(yearBorn_data), groupFlag = "_t")
#'
#' @export
rename_cols <- function(x, ...){

### quick check for x
stopifnot("\nThe object 'x' you supplied is not of type vector." = is.vector(x))

### proceed otherwise
## extract other specified arguments & set defaults
dots <- list(...)
# set groupFlag
groupFlag <-
  if (!is.null(dots[["groupFlag"]])) {
  dots[["groupFlag"]] }
  else {
    "_w\\d$"
  }
# check that groupFlag ends in a digit (if not add)
groupFlag <- ifelse(grepl(escape_punct("\\d$"), groupFlag), groupFlag, paste0(groupFlag, "\\d$"))
# check that groupFlag as underscore
groupFlag <- ifelse(grepl(pattern = "^_", x = groupFlag), groupFlag, paste0("_",groupFlag))

## remove group/time flag + return vector of stem names
gsub(x = x, pattern = groupFlag, replacement = "")

}
