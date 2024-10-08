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
stopifnot("\nThe data set you supplied is not a tibble or data frame."=
any(class(dataSet) %in% c("tbl_df","tbl","data.frame")))

### otherwise, proceed
## reorder
# if the column exists reorder
if(colReorder %in% names(dataSet)){
	dataSet[order(match(dataSet[[colReorder]],newOrder)),]
# else stop
} else{
	stop("\nThe supplied column to reorder does not exist in the supplied dataSet.")
	}
}
