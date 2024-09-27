#' Function to create factor labels
#'
#' @description This function can be used to create factor labels for variables
#' in a data frame or tibble.
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
stopifnot("\nThe key you supplied is not a tibble or data frame." = class(key) %in% c("tbl_df","tbl","data.frame"))

### otherwise, proceed
## extract other specified arguments (these are optional)
dots <- list(...)
# set variableName_col
variableName_col <- if (!is.null(dots[["variableName_col"]])) dots[["variableName_col"]] else "variableName"
# set valueLabels_col
valueLabels_col <- if (!is.null(dots[["valueLabels_col"]])) dots[["valueLabels_col"]] else "valueLabels"
# set addDelim
addDelim <- if (!is.null(dots[["addDelim"]])) dots[["addDelim"]] else ";"

#### begin main transformation
## extract variable name
x_name <- rename_cols(x)

### check that variable name exists in key
stopifnot("\nThe column name you specified is not in the supplied data set." = x_name %in% key[[variableName_col]])

## subset key
key <- key[key[variableName_col] == x_name,][[valueLabels_col]]

## return vector of values
vlulist(key, addDelim = addDelim)
}
