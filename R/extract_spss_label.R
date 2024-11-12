#' Function for extracting variable labels from an SPSS data set
#'
#' @description This function can be used to extract question/variable labels from
#' an SPSS data set.
#'
#' @importFrom dplyr bind_rows
#'
#' @param df a tibble or data frame.
#' @param vars character with name of variable in df. Default is set to all, and all variable
#' labels will be extracted
#'
#' @returns A data frame with two columns: variable names (column 1) and labels (column 2).
#'
#' @author Ama Nyame-Mensah
#'
#' @export
extract_spss_label <- function(df, vars){

### quick check on df parameter
stopifnot("\nThe data set you supplied is not a tibble or data frame." =
            any(class(df) %in% c("tbl_df","tbl","data.frame")))


### check vars parameter
## first escape
vars <- escape_punct(vars)

## check they all exist
stopifnot("\nOne or more variables specified do not exist in the supplied data set.\n" =
            sum(grepl(paste0("^",escape_punct(vars),"$",collapse = "|"),
            names(df))) == length(escape_punct(vars)))

### other setup
## get variable indices
varIndices <- grep(paste0("^", vars,"$", collapse = "|"), names(df))

### extract labels
all_labels <- lapply(varIndices, \(x){
  currentLabel <- attributes(df[[x]])$"label"
  # return data frame with varName and its label, use "CHECK" if
  # label is missing
  data.frame(varName = gsub("\\\\", "", names(df)[x]),
             varLabel = ifelse(is.null(currentLabel),"CHECK", currentLabel))
})

## combine into data frame
dplyr::bind_rows(all_labels)
}
