#' Function to perform a general merge list of data frames
#'
#' @description
#' This is a generalized version of merge_dataframes_lists as key ids are chosen from data sets
#' that are merged. NOTE: by default no sorting takes place and the key data frame is the first
#' data frame that appears in the list
#'
#' @param dfLists list of data frames or tibbles.
#'
#' @examples
#' # Example
#' dataFrame1 <- data.frame(CERPID = sample(paste0("000", 1:9), size = 9, replace = FALSE),
#' student = sample(1:4, size = 9, replace = TRUE))
#' dataFrame2 <- data.frame(gender = sample(c("woman","man", "non-binary"), size = 9, replace = TRUE),
#' CERPID = sample(paste0("000", 1:9), size = 9, replace = FALSE))
#' merge_dfList(list(dataFrame1, dataFrame2))
#'
#' @export
merge_dfList <- function(dfLists) {

### quick check for list of data frames/tibbles
stopifnot("\nThe list of data frames/tibbles you supplied has at least one object that is not a tibble or data frame."
          = all(vapply(dfLists, \(x) class(x) %in% c("tbl_df","tbl","data.frame"), logical(1))))

### reduce into one data frame
Reduce(function(x,y) merge(x = x, y = y, all.x  = TRUE, sort = FALSE), dfLists)

}
