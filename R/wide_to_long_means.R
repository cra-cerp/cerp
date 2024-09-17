#' Calculate means for longitudinal variables with the same variable stem.
#'
#' @description
#' This function is a wrapper for calculating wide to long means for many variables with the same (variable) stem.
#' NOTE: By default, listwise deletion is set to TRUE (removing any records with NA values) and the default
#' wave/group flag is _w. Both of these can be modified using the listWise and groupFlag inputs, respectively
#' (see below).
#'
#' @importFrom stats na.omit
#' @importFrom dplyr bind_rows
#'
#' @param x a character vector of variable stems.
#' @param dataSet a tibble or data frame.
#' @param waves the number of waves/measurement occasions. Note: Wave numbers must be whole numbers and
#' correspond to the wave/group flag that appears in the data set (e.g., variable_w1 corresponds to wave
#' 1, whereas variable_w3, corresponds to wave 3).
#' @param \dots Additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{groupFlag}: An optional parameter that is a character vector containing the group
#'  or time flag pattern to identify each variable stem. The default is "_w". This default can be
#'  overridden by specifying another group or time flag pattern. See example 2 below.
#'  \item \code{listWise}: An optional parameter that when set to TRUE, listwise deletion is used.
#'  \item \code{wave_col_names}: Optional parameters that can be used to pass new field/column names to
#'  the table. For example, if you have two waves of data and you do not want pre_n and post_n to be
#'  printed, imply add wave_col_name1 = "wave1" and add wave_col_name2 = "wave2" and wave1_n and wave_2
#'  will be printed, respectively. See example 2 below for an example of how to use this parameter.
#'  }
#'
#' @returns A tibble.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with default group/time flag and listWise (deletion)
#' wide_tabl <- data.frame(belong_outsider_w1 = sample(1:5, replace = TRUE, size = 10),
#' belong_outsider_w2 = sample(c(NA,1:5), replace = TRUE, size = 10))
#' wide_to_long_means(x = "belong_outsider", dataSet = wide_tabl, waves = c(1,2))
#'
#' # Example with user-specified group/time flag, listWise (deletion) set to FALSE,
#' # custom table headers are used, and multiple variables are considered
#' wide_n_tabl2 <- data.frame(belong_outsider_time1 = sample(1:5, replace = TRUE, size = 10),
#' belong_outsider_time3 = sample(c(NA,1:5), replace = TRUE, size = 10),
#' belong_welcomed_time1 = sample(c(NA,1:5), replace = TRUE, size = 10),
#' belong_welcomed_time3 = sample(c(NA,1:5), replace = TRUE, size = 10))
#' wide_to_long_means(x = c("belong_welcomed","belong_outsider"), dataSet = wide_n_tabl2,
#' waves = c(1L,3L), groupFlag = "_time",listWise = FALSE,wave_col_name1 = "Wave1",
#' wave_col_name2 = "Wave3")
#'
#' @export
wide_to_long_means <- function(x, dataSet, waves, ...){

### quick check on required parameters
stopifnot("\nThe data set you supplied is not a tibble or data frame." = (sum(grepl("tbl_df|tbl|data.frame", class(dataSet))) > 0),
          "\nNo columns in the data set you supplied have the variable stem you provided." = (sum(grepl(paste0("^",x, collapse = "|"), names(dataSet))) > 0),
          "\nSupplied waves are not whole numbers." = all(sapply(waves, function(x){x %% 1 == 0})))

### otherwise proceed
## extract other specified arguments (these are optional)
dots_list <- list(...)
if(length(dots_list) != 0){
  dots <- unlist(dots_list)
  groupFlag <- dots[grep("^groupflag$", tolower(names(dots)))]
  listWise <- dots[grep("^listwise$", tolower(names(dots)))]
  wave_col_names <- dots[grep("^wave_col_name", tolower(names(dots)))]
} else{
  dots <- NULL
}

## set these inputs to null/pre-determined string
groupFlag <- if(is.null(dots) | !any(grepl("^groupflag$", tolower(names(dots))))){paste0("w")} else{groupFlag}
groupFlagWaves <- if(is.null(dots) | !any(grepl("^groupflag$", tolower(names(dots))))){paste0("w", waves)}
else{paste0(groupFlag, waves)}
listWise <- if(is.null(dots) | !any(grepl("^listwise$", tolower(names(dots))))){TRUE}else{listWise}
wave_col_names <- if(is.null(dots) | !any(grepl("^wave_col_name", tolower(names(dots))))){NULL}else{wave_col_names}

## do some checks
# remove leading underscore if exists in groupFlag/groupFlagWaves
groupFlag <- gsub(pattern = "^\\_", replacement = "", groupFlag)
# groupFlagWaves
groupFlagWaves <- sapply(groupFlagWaves, function(x) gsub("^\\_","", x),USE.NAMES = FALSE)
# and wave_col_names
if(!is.null(wave_col_names)){wave_col_names <- sapply(wave_col_names, function(x) gsub("^\\_","", x),USE.NAMES = FALSE)}
# finally check that the wave/group flags exist. If an error STOP
if(sum(grepl(paste0(groupFlagWaves,collapse = "|"), names(dataSet))) == 0)
{stop("\nAt least one of your variables does not have the same group/wave flag. Please check your dataset.")}

### Table construction
## iterate over all variable stems
full_mean_tabl <- lapply(x, function(y){
  # find variables
  xFull <- paste0("^",y, "_" ,groupFlagWaves,"$", collapse = "|")
  # subset the data set by those variables
  subdat <- dataSet[, grepl(xFull, names(dataSet)) & !grepl("_TEXT$", names(dataSet))]

  # if listWise is set to TRUE (default); remove observations through list wise deletion
  if(listWise){subdat <- stats::na.omit(subdat)}

  # convert to numeric type
  subdat2 <- data.frame(lapply(subdat, as.numeric))
  # count number of observations
  mean_tabl <- apply(subdat2, 2, function(x) {mean(x, na.rm = TRUE)})

  # replace column names: first set to numbers
  names(mean_tabl) <- gsub(pattern = paste0(y,"_",groupFlag),
                           replacement = "", names(mean_tabl))

  # then check if any were user supplied
  if(length(wave_col_names) > 0){
    names(mean_tabl) <- paste0(wave_col_names, "_mean")
  } else{
    # if not: use default names for table
    names(mean_tabl)[grepl("^1$", names(mean_tabl))] <- "Pre_n"
    names(mean_tabl)[grepl("^2$", names(mean_tabl))] <- "Post_n"
    names(mean_tabl)[grepl("^3$", names(mean_tabl))] <- "Follow-Up_n"
  }

  # add variable name tag
  mean_tabl <- c(name = y, mean_tabl)
  })

### row bind using dplyr's smart bind + return tibble
do.call(dplyr::bind_rows, full_mean_tabl)

}
