#' Function to create factor labels
#'
#' @description This function can be used to create factor labels for variables
#' in a data frame or tibble.
#'
#' @importFrom cerp rename_cols
#' @importFrom cerp vlulist
#'
#' @param x a variable name WITHOUT group/time flag.
#' @param key a tibble or data frame containing two columns 1) variable names (without group/time flag),
#' and 2) variable values. NOTE: Variable values should be concatenated together using a semi-colon
#' (e.g., "one;two;three").
#' @param \dots additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{variableName_col}: An optional argument. This parameter is used to specify the name of the
#'  column/field in the codebook to extract variable names from.
#'  \item \code{valueLabels_col}: An optional argument. This parameter is used to specify the name of the
#'  column/field in the codebook to extract value labels from.
#'}
#'
#' @returns A vector of values.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with data frame using default field names: variableName
#' # and variableValues
#' factor_labels_dat <- data.frame(variableName = c("sex", "race_black"),
#' valueLabels = c("Woman;Man;Agender;Something Else", "Unselected;Selected"))
#' factor_labels(x = "race_black", key = factor_labels_dat)
#'
#' # Example with data frame using user-specified field names: var_names
#' # and value_labels
#' factor_labels_dat2 <- data.frame(var_names = c("edFund_fedLoan","belong_welcomed"),
#' value_labels = c("Unselected;Selected",
#' "Strongly disagree;Somewhat disagree;Neither disagree nor agree;Somewhat agree;Strongly agree"))
#' factor_labels(x = "belong_welcomed", key = factor_labels_dat2,
#' variableName_col = "var_names", valueLabels_col = "value_labels")
#'
#' @export
factor_labels <- function(x, key, ...){

### quick check key
stopifnot("\nThe key you supplied is not a tibble or data frame." = (sum(grepl("tbl_df|tbl|data.frame", class(key))) > 0))

### otherwise, proceed
## extract from dots
dots_list <- list(...)
if(length(dots_list) != 0){
  dots <- unlist(dots_list)
  variableName_col <- dots[grepl("^variablename_col$", tolower(names(dots)))]
  valueLabels_col <- dots[grepl("^valuelabels_col$", tolower(names(dots)))]
} else{
  dots <- NULL
}

## otherwise set these inputs to null/pre-determined string
variableName_col <- if(is.null(dots) | !any(grepl("^variablename_col$", tolower(names(dots))))){"variableName"} else{variableName_col}
valueLabels_col <- if(is.null(dots) | !any(grepl("^valuelabels_col$", tolower(names(dots))))){"valueLabels"} else{valueLabels_col}

#### begin main transformation
## extract variable name
x_name <- cerp::rename_cols(x)

### check that variable name exists in key
stopifnot("\nThe column name you specified is not in the supplied data set." = (sum(grepl(paste0("^",x_name,"$"), key[[variableName_col]])) > 0))

## subset key
key <- data.frame(key[grepl(paste0("^",x_name, "$"), key[[variableName_col]]),])

## return vector of values
cerp::vlulist(key[[valueLabels_col]])

}
