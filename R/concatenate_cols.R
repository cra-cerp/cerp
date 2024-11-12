#' A function to concatenate the rows of supplied data frame columns
#'
#' @description This function is similar to tidyr's unite() function and can be used to
#' concatenate the rows of supplied data frame columns.
#'
#' @importFrom purrr pmap_chr
#'
#' @param df A tibble or data frame.
#' @param vars A vector of column names.
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
#' concat_example$concat_cols <- concatenate_cols(df = concat_example,
#' vars = c("id", "date", "participantCode"))
#'
#' @export

concatenate_cols <- function(df, vars){
	## quick object checks
	# df (check both class and that there is at least one row of data)
	stopifnot("\nThe df you supplied is not of class tibble or data frame." =
	any(class(df) %in% c("tbl_df","tbl","data.frame")),
	"\nThe df you supplied does not contain any observations." =
	prod(dim(df)) > 0)
	# vars (check at least one supplied and that all appear in the data)
	stopifnot("\nPlease supply at least one column name." = length(vars) > 0L,
				 "\nAt least one supplied column name does not exist in the supplied df." =
				 all(vars %in% names(df)))

	## concatenate
	# do.call(paste0, df[, vars, drop = TRUE])
	purrr::pmap_chr(df[, vars, drop = FALSE], paste0)

}
