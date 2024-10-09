#' A function to concatenate the rows of supplied data frame columns
#'
#' @description This function is similar to tidyr's unite() function and can be used to
#' concatenate the rows of supplied data frame columns.
#'
#' @param dataset A tibble or data frame.
#' @param column_names A vector of column names.
#'
#' @returns A vector.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with one key column (want to keep earliest date)
#' concat_example <- data.frame(id = c("id_3","id_1","id_2","id_2","id_1","id_3"),
#' date = as.Date(c("2024-05-01","2024-08-01","2024-02-14","2024-05-01","2024-05-01","2024-01-01")),
#' participantCode = c("NSF REU", "CAHSI", "DREU","DREU", "CAHSI", "NSF REU"))
#'
#' concat_example$concat_cols <- concatenate_cols(dataSet = concat_example,
#' column_names = c("id", "date", "participantCode"))
#'
#' @export

concatenate_cols <- function(dataSet, column_names){
	## quick object checks
	# dataSet (check both class and that there is at least one row of data)
	stopifnot("\nThe dataSet you supplied is not of class tibble or data frame." =
	any(class(dataSet) %in% c("tbl_df","tbl","data.frame")),
	"\nThe dataSet you supplied does not contain any observations." =
	prod(dim(dataSet)) > 0)
	# column_names (check at least one supplied and that all appear in the data)
	stopifnot("\nPlease supply at least one column name." = length(column_names) > 0L,
				 "\nAt least one supplied column name does not exist in the supplied dataSet." =
				 all(column_names %in% names(dataSet)))

	## warning (coerce to data.frame)
	if(any(class(dataSet) %in% c("tbl_df","tbl"))){
	warning("Coercing dataSet to data.frame.", immediate = TRUE)
	dataSet <- as.data.frame(dataSet)
	}

	## concatenate
	do.call(paste0, dataSet[, column_names, drop = TRUE])
}
