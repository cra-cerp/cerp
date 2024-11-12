#' Function to create indicator wave column
#'
#' @description
#' This is a generalized function for extracting time or group flags for variables.
#'
#' @param x a vector to split
#' @param \dots additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{groupFlag}: An optional parameter that is a character vector containing the group
#'  or time flag pattern to identify each variable stem. The default is "_w". This default can be
#'  overridden by specifying another group or time flag pattern. See example 2 below.
#'  }
#'
#' @returns A vector of values. If a wave/grop flag is found, the flag is returned. Otherwise,
#' globalVar is returned.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with default group/time flag
#' temp_dat <- data.frame(variable_name = c("sex_w1","sex_w2","sex"))
#' temp_dat$timeFlag <- timeFlag_cols(x = temp_dat$variable_name)
#' temp_dat
#'
#' # Example with user-specified group/time flag
#' temp_dat2 <- data.frame(variable_name = c("highestDeg_it_group1", "csBackground_newProgLang",
#' "mentorSupport_group2"))
#' temp_dat2$groupFlag <- timeFlag_cols(x = temp_dat2$variable_name, groupFlag = "_group")
#' temp_dat2
#'
#' @export
timeFlag_cols <- function(x, ...){

### quick check for vector
stopifnot("\nThe x variable you supplied is not of type vector." = is.vector(x))

### otherwise, proceed
## extract other specified arguments & set defaults
dots <- list(...)
# set groupFlag
groupFlag <-
  if (!is.null(dots[["groupFlag"]])) {
    dots[["groupFlag"]] }
  else {
    "_w\\d$"
  }
# check that groupFlag ends in a digit
groupFlag <- ifelse(grepl(escape_punct("\\d$"), groupFlag), groupFlag, paste0(groupFlag, "\\d$"))
# check that groupFlag has an underscore
groupFlag <- ifelse(grepl(pattern = "^_", x = groupFlag), groupFlag, paste0("_",groupFlag))

### extract time flags
result <- vapply(x, find_groupFlag, groupFlag = groupFlag, FUN.VALUE = character(1))
# return result
result
}

## helper function to find group/time flag
find_groupFlag <- function(x, groupFlag) {
  if(grepl(x = x, pattern = groupFlag)){
    # if found replace with just the time/groupFlag
    toReplace <- strsplit(x = x, split = groupFlag)
    gsub(pattern = paste0(toReplace,"_"), replacement = "", x = x)
  } else{
    # default return globalVar
    "globalVar"
  }
}
