#' General function for creating pushed/global vector of values
#'
#' @description This function can be used to find and retain the most recent
#' response for a record.
#'
#' @importFrom mgsub mgsub
#'
#' @param dataSet A tibble or data frame object.
#' @param listVarStems A character vector of unique variable (name) stems.
#' @param \dots additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{groupFlag}: An optional parameter that is a character vector containing the group
#'  or time flag pattern to identify each variable stem. The default is "_w". This default can be
#'  overridden by specifying another group or time flag pattern. See example 2 below.
#'  }
#'
#' @author Ama Nyame-Mensah
#'
#' @returns A character vector that is column-binded to the original supplied dataSet. NOTE: The most recent
#' (i.e., LATEST) non-null, non-missing value of the LATEST group/time flag is retained.
#'
#' @examples
#' # Example with default group/time flag
#' sexBirth_data
#' createGlobalVars(dataSet = sexBirth_data, listVarStems = "sex")
#'
#' # Example with user-specified group/time flag (note the time flag pattern is "_t")
#' yearBorn_data
#' createGlobalVars(dataSet = yearBorn_data, listVarStems = "yearBorn", groupFlag = "_t")
#'
#' @export
createGlobalVars <- function(dataSet, listVarStems,...){

### quick check on required parameters
stopifnot("\nThe data set you supplied is not a tibble or data frame." = any(class(dataSet) %in% c("tbl_df","tbl","data.frame")),
          "\nThe variable stems you supplied are not in a vector." = is.vector(listVarStems))

### proceed otherwise
## extract other specified arguments (these are optional)
dots <- list(...)
# set groupFlag
groupFlag <- if (!is.null(dots[["groupFlag"]])) dots[["groupFlag"]] else "_w\\d$"
# check that groupFlag ends in a digit
groupFlag <- ifelse(grepl(escape_punct("\\d$"), groupFlag), groupFlag, paste0(groupFlag, "\\d$"))
# check that groupFlag as underscore
groupFlag <- ifelse(grepl(pattern = "^_", x = groupFlag), groupFlag, paste0("_",groupFlag))
# create groupFlag shortened
groupFlagShortened <- ifelse(grepl("^_w",groupFlag), "_w", gsub("\\d$","", groupFlag, fixed = TRUE))


## general function for finding most up to date value to be pushed
# NOTE: if NA/blank across the board, then NA is returned
newVarValues <- function(dataSetSub, rowNum, stemName){
  currentRow <- dataSetSub[rowNum,]
  currentRowUpdate <- unlist(Filter(function(x) !any(is.na(x)), currentRow))
  currentRowUpdate <- currentRowUpdate[currentRowUpdate != ""]
  maxWave <- mgsub::mgsub(names(currentRowUpdate),
                          pattern = rep(paste0(stemName, groupFlagShortened),
                                        times = length(names(currentRowUpdate))),
                          rep("", times = length(names(currentRowUpdate))))

  maxWave <- ifelse(length(maxWave) == 0, NA, max(maxWave))

  if(length(unique(currentRowUpdate)) == 1){
    currentRowUpdate[[1]]
    }else if(length(unique(currentRowUpdate)) > 1){
      currentRowUpdate[[paste0(stemName, groupFlagShortened,maxWave)]]
    }else{
      NA
    }
}

## general function for creating pushed/global vector of values
checkUpdate <- function(dataSet, stemName){
  # iterate over list of stem variable names
  lapply(stemName, \(x){
    columnNames <- names(dataSet)[grepl(paste0("^",x, groupFlag), names(dataSet))]
    subDat <- dataSet[columnNames]
    newVar <- lapply(seq_len(nrow(subDat)), newVarValues, dataSetSub = subDat, stemName = x)
    newVar <- lapply(newVar, function(x) ifelse(length(x) == 0, NA, x))
    unlist(newVar)
    })
}

## main manipulation
new_GlobalVars <- data.frame(checkUpdate(dataSet, unlist(listVarStems)))
names(new_GlobalVars) <- unlist(listVarStems)

## check + stop if any names already exist
if(any(names(new_GlobalVars) %in% names(dataSet))){
  stop(paste0("\nThe following variables already exist in the supplied data set:\n",
              paste0(names(new_GlobalVars)[which(names(new_GlobalVars) %in% names(dataSet))], collapse = "\n")))
}

## otherwise, bind new column(s) to original passed data set
cbind.data.frame(dataSet, new_GlobalVars)

}
