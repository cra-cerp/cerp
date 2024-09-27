#' Wrapper for recoding values to value labels in a tibble or data frame.
#'
#' @description
#' This function is a wrapper for recoding variable values and applying labels. It was designed to
#' work with case_match from the dplyr package.
#'
#' @param value A character vector of values to recoded.
#' @param label A character vector of labels to replace values.
#' @param \dots additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{incluMissing}: An optional parameter. Set it to TRUE to replace missing values (e.g., -99) to
#'  "missing". Default is FALSE. NOTE: This is not to be confused with NA in R. This is purely for actual
#'  values that you would like to recode with the label 'missing'. To recode NA values to missing, add NA
#'  to the vector of values supplied through the value input. (See the second example below for an example
#'  of passing NA.) Finally, if you would like to keep default values but DO NOT include the value in the
#'  list supplied to value or label, use the .default input from case_match.
#'  \item \code{missingValue}: If incluMissing is set to TRUE, you can specify what value (e.g., -99) should
#'  be recoded to "Missing". NOTE: Only use whole numbers.
#' }
#'
#' @returns A list of two sided formulas.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with default settings/no missing specified
#' recode_apply_dat <- data.frame(rank = sample(1:10, size = 10, replace = FALSE))
#' recode_apply_dat$rank_labelled <- dplyr::case_match(recode_apply_dat$rank,
#' !!!recode_key_apply(value = 1:10, label = c("first", "second", "third", "fourth",
#' "fifth", "sixth", "seventh", "eighth", "ninth","tenth")))
#' recode_apply_dat
#'
#' # Example with NA values recoded to system missing
#' recode_apply_dat2 <- data.frame(gender = sample(c(1:3,NA), size = 10, replace = TRUE))
#' recode_apply_dat2$gender_labelled <- dplyr::case_match(recode_apply_dat2$gender,
#' !!!recode_key_apply(value = c(1:3,NA), label = c("woman", "man", "non-binary", "system missing")))
#'
#' # Example with user missing values recoded to missing
#' recode_apply_dat3 <- data.frame(race = sample(c(1:4,NA,4444), size = 15, replace = TRUE))
#' recode_apply_dat3$race_labelled <- dplyr::case_match(recode_apply_dat3$race,
#' !!!recode_key_apply(value = c(1:4,NA), label = c("Black/African American", "Asian",
#' "Hispanic/Latinx", "Indigenous", "System Missing"), incluMissing = TRUE, missingValue = 4444))
#'
#' @export
recode_key_apply <- function(value, label, ...) {

### quick check on object type for value and label
stopifnot("\nThe value parameter you supplied is not of type vector." = is.vector(value),
          "\nThe label parameter you supplied is not of type vector." = is.vector(label))

### otherwise, proceed
## extract other specified arguments (these are optional)
dots <- list(...)
# set incluMissing
incluMissing <- if (!is.null(dots[["incluMissing"]])) dots[["incluMissing"]] else FALSE
# set missingValue
missingValue <- if (!is.null(dots[["missingValue"]])) dots[["missingValue"]] else NA

## check whether to add label to missing values
if(grepl("true|t|y|yes|1", tolower(incluMissing))){
	# check missing value
	missingValue <- ifelse(missingValue == "NA", NA, missingValue)
    value <- c(value, missingValue)
    label <- c(label, "Missing")
}

## print two sided formulas
mapply(function(formula, x, y) formula = bquote(.(x) ~ .(y)), x = value, y = label)

}
