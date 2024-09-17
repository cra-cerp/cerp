#' Function for extracting question/variable labels for variables/columns from an SPSS data set
#'
#' @importFrom tibble is_tibble
#' @importFrom cerp escape_punct
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
variableName <- cerp::escape_punct(variableName)

### quick check on required parameters
stopifnot("\nThe data set you supplied is not a tibble or data frame." = (sum(grepl("tbl_df|tbl|data.frame", class(dataSet))) > 0),
          "\nOne or more variables specified do not exist in the supplied data set.\n" = all(grepl(paste0("^", variableName,"$", collapse = "|"), names(dataSet))))

### otherwise, proceed
## iterate over all specified variables
all_labels <- lapply(variableName, function(variableName){
  # find variable index
  varIndex <- grep(paste0("^", variableName,"$"), names(dataSet))
  # extract attribute
  currentLabel <- attributes(dataSet[[varIndex]])$"label"
  # NOTE: varLabel is reset to CHECK if label is missing
  data.frame(varName = variableName, varLabel = ifelse(length(currentLabel) == 1, currentLabel, "CHECK"))
})

## combine into data frame
do.call(rbind, all_labels)

}
