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
## extract from dots
dots_list <- list(...)
if(length(dots_list) != 0){
  dots <- unlist(dots_list)
  groupFlag <- dots[grep("^groupflag$", tolower(names(dots)))]
} else{
  dots <- NULL
}

## set these inputs to null/pre-determined string
groupFlag <- if(is.null(dots) | !any(grepl("^groupflag$", tolower(names(dots))))){"_w\\d$"} else{groupFlag}
## check that groupFlag ends in a digit
groupFlag <- ifelse(grepl(cerp::escape_punct("\\d$"), groupFlag), groupFlag, paste0(groupFlag, "\\d$"))
## check that groupFlag as underscore
groupFlag <- ifelse(grepl(pattern = "^_", x = groupFlag), groupFlag, paste0("_",groupFlag))

### iterate over x to find group/time flags
sapply(x, function(find_groupFlag){
	# check for time/group flag
	if(grepl(x = find_groupFlag, pattern = groupFlag)){
		# if found replace with just the time/groupFlag
		toReplace <- unlist(strsplit(x = find_groupFlag, split = groupFlag))
		gsub(pattern = paste0(toReplace,"_"), replacement = "", x = find_groupFlag)
	} else{
		# other return globalVar
		"globalVar"
		}
})

}
