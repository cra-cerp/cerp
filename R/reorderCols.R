#' Reorder column rows in a data frame or tibble
#'
#' @description
#' This function allows the user to specify the order of rows in a column by supplying a vector of
#' values (newOrder).
#'
#' @param dataSet A tibble or data frame.
#' @param colReorder The name of the column to reorder.
#' @param newOrder A character vector containing a user-specified order for column string/values.
#'
#' @returns A tibble/data frame.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' outOfOrder <- data.frame(values = c("zucchini", "banana", "apple", "carrot"),
#' letter = c("z", "b", "a","c"))
#' inOrder <- outOfOrder
#' reorderCols(dataSet = outOfOrder, colReorder = "values",
#' newOrder = c( "apple","carrot","banana","zucchini"))
#'
#' @export
reorderCols <- function(dataSet, colReorder, newOrder){

### quick check for list of data frames/tibbles
stopifnot("\nThe data set you supplied is not a tibble or data frame."= (sum(grepl("tbl_df|tbl|data.frame", class(dataSet))) > 0))

### otherwise, proceed
## find column name
colReorder2 <- as.character(substitute(colReorder))
## check if exists in data set
if(sum(grepl(paste0("^", colReorder2, "$"), names(dataSet))) == 0){
  # if not, pull pasted original name
  colReorder2 <- paste0(colReorder)
}

## reorder rows in column and return tibble/data frame
dataSet[order(match(dataSet[[colReorder2]],newOrder)),]

}
