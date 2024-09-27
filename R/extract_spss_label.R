#' Function for extracting variable labels from an SPSS data set
#'
#' @description This function can be used to extract question/variable labels from
#' an SPSS data set.
#'
#' @param dataSet a tibble or data frame.
#' @param variableName character with name of variable in dataSet.
#'
#' @returns A data frame with two columns: variable names (column 1) and labels (column 2).
#'
#' @author Ama Nyame-Mensah
#'
#' @export
extract_spss_label <- function(dataSet, variableName){

### escape punctuation
variableName <- escape_punct(variableName)

### quick check on required parameters
stopifnot("\nThe data set you supplied is not a tibble or data frame." = class(dataSet) %in% c("tbl_df","tbl","data.frame"),
          "\nOne or more variables specified do not exist in the supplied data set.\n" = all(variableName %in% names(dataSet)))

### otherwise, proceed
## iterate over all specified variables
all_labels <- lapply(variableName, \(variableName){
  # find variable index
  varIndex <- grep(paste0("^", variableName,"$"), names(dataSet))
  # extract attribute
  currentLabel <- attributes(dataSet[[varIndex]])$"label"
  # NOTE: varLabel is reset to CHECK if label is missing
  data.frame(varName = variableName, varLabel = ifelse(length(currentLabel) != 1, currentLabel, "CHECK"))
})

## combine into data frame
do.call(rbind, all_labels)

}
