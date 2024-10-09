#' Function to create factor levels
#'
#' @description This function can be used to create factor levels for variables
#' in a data frame or tibble.
#'
#' @param x a variable name WITHOUT group/time flag.
#' @param key a tibble or data frame containing two columns 1) variable names (without group/time flag),
#' and 2) variable values. NOTE: Variable values should be concatenated together using a comma
#' (e.g., "1,2,3").
#' @param \dots additional parameters to pass to function.These include: variableName_col (an optional input
#' you can specify; variable names will be extracted from this column/field) and variableValues_col (an optional
#' input you can specify; variable values will be extracted from this column/field).
#' \itemize{
#'  \item \code{variableName_col}: An optional argument. This parameter is used to specify the name of the
#'  column/field in the codebook to extract variable names from.
#'  \item \code{variableValues_col}: An optional argument. This parameter is used to specify the name of the
#'  column/field in the codebook to extract variable values from.
#'}
#'
#' @returns A vector of values.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with data frame using default field names: variableName and variableValues
#' factor_levels_dat <- data.frame(variableName = c("sex", "race_black"),
#' variableValues = c("1,2,3,4", "0,1"))
#' factor_levels(x = "sex", key = factor_levels_dat)
#'
#' # Example with data frame using user-specified field names: var_names and var_values
#' factor_levels_dat2 <- data.frame(var_names = c("edFund_fedLoan","belong_welcomed"),
#' var_values = c("0,1", "1,2,3,4,5"))
#' factor_levels(x = "edFund_fedLoan", key = factor_levels_dat2, variableName_col = "var_names",
#' variableValues_col = "var_values")
#'
#' @export
factor_levels <- function(x, key, ...){

### quick check key
stopifnot("\nThe key you supplied is not a tibble or data frame." = any(class(key) %in% c("tbl_df","tbl","data.frame")))

### otherwise, proceed
## extract other specified arguments & set defaults
dots <- list(...)
# set variableName_col
variableName_col <-
  if (!is.null(dots[["variableName_col"]])) {
  dots[["variableName_col"]] }
  else {
  "variableName"
    }
# set variableValues_col
variableValues_col <-
  if (!is.null(dots[["variableValues_col"]])) {
    dots[["variableValues_col"]] }
  else {
    "variableValues"
  }
# set addDelim
addDelim <-
  if (!is.null(dots[["addDelim"]])) {
    dots[["addDelim"]] }
  else {
    ","
  }

#### begin main transformation
## extract variable name
x_name <- rename_cols(x)

### check that variable name exists in key
stopifnot("\nThe column name you specified is not in the supplied data set." = x_name %in% key[[variableName_col]])

## subset key
key <- key[key[variableName_col] == x_name,][[variableValues_col]]

## return vector of values
vulist(key, addDelim = addDelim)

}
