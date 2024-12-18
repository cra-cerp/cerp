#' Function to merge list of data frames together on ONE key column.
#'
#' @param dfLists A list of data frames or tibbles
#' @param \dots Additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{key (or id)}: An optional parameter. A user-specified key column to merge on.
#'  By default the column is set to "CERPID".
#'  }
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example
#' all_dfs <- list(df_1 = data.frame(id_key = c(3,1,2),
#' employee = c("heather", "burcin", "evelyn")),
#' df_2 = data.frame(title = c("director", "associate director",
#' "data manager"), id_key = c(1,3,2)))
#' merge_dataframes_lists(dfLists = all_dfs, keyColumn = "id_key")
#'
#' @export
merge_dataframes_lists <- function(dfLists, ...){

### quick check for list of data frames/tibbles
stopifnot("\nThe list of data frames/tibbles you supplied has at least one object that is not a tibble or data frame."
          = all(vapply(dfLists, \(x) any(class(x) %in% c("tbl_df","tbl","data.frame")), logical(1))))

### proceed otherwise
## extract other specified arguments & set defaults
dots <- list(...)
# set keyColumn
keyColumn <- if (!is.null(dots[["keyColumn"]])) {
  dots[["keyColumn"]]
  }else {
    "CERPID"
  }

## quick check for key column in all objects
stopifnot("\nAt least one object does not contain the key column."
          = all(vapply(dfLists, \(x) keyColumn %in% names(x), logical(1))))

## reduce into one data frame
Reduce(function(x,y) merge(x = x, y = y, by = keyColumn, all  = TRUE, sort = FALSE), dfLists)

}
