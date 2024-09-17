#' Match and return value.
#'
#' @description
#' This function is similar to xlookup in Excel.
#' It searches for a certain value in a column and then returns a value from a different column
#' in the same row. Note: NA is returned when no match is found
#'
#' @param searchVal A character vector containing a string or value to search for.
#' @param lookUpDataFrame A tibble or data frame.
#' @param lookUpCol The name of the column to search for searchVal.
#' @param returnCol The name of the column with the value to return.
#'
#' @returns A vector.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' race_key <- data.frame(value = 1:5, label = c("Black/African American", "Asian/Asian American",
#' "Indigenous", "Native Hawaiian/Pacific Islander", "No entry"))
#' race_tabl <- data.frame(race_ethnicity = sample(1:5, size = 10, replace = TRUE))
#' race_tabl$race_eth_label <- match_return(race_tabl$race_ethnicity, lookUpDataFrame = race_key,
#' lookUpCol = "value", returnCol = "label")
#'
#' @export
match_return <- function(searchVal, lookUpDataFrame, lookUpCol, returnCol){

### quick check for data frame and lookup/return columns
stopifnot("\nThe look-up data frame you supplied is not a tibble or data frame." = (sum(grepl("tbl_df|tbl|data.frame", class(lookUpDataFrame))) > 0),
          "\nThe look-up column you supplied does not exist in the supplied data frame" = (sum(grepl(lookUpCol, names(lookUpDataFrame))) > 0),
          "\nThe return column you supplied does not exist in the supplied data frame" = (sum(grepl(returnCol, names(lookUpDataFrame))) > 0))

### otherwise, proceed
## iterate over searchVal
sapply(searchVal, function(toSearch){
  # search for match and return
  lookUpDataFrame[match(escape_punct(toSearch), lookUpDataFrame[[lookUpCol]]), returnCol]
})

}
