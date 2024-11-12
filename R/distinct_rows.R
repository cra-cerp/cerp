#' A function to retain/keep unique rows
#'
#' @description This function is similar to dplyr's distinct() function and can be used to
#' retain distinct/unique rows.
#'
#' @param df A tibble or data frame.
#' @param column A key column.
#' @param retain Which record to retain (i.e., first/last). Parameter governs whether the first or last
#' instance of each duplicate is retained. NOTE: if you need to keep the first or last instance of a
#' duplicate based on some other variable (like a date), first sort the data set by that column and then
#' use the function.
#' @param keep_allVars TRUE/FALSE; if TRUE all columns are retained, else only the key column is retained.
#'
#' @param \dots additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{columnN}: An optional parameter where "N" corresponds to a number.
#'  Add additional column variables followed by a number (e.g., column2, column3)
#'  to consider additional key columns when retaining distinct/unique rows.
#'  }
#'
#' @returns A data frame.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with one key column (want to keep earliest date)
#' distinct_example <- data.frame(id = c("id_3","id_1","id_2","id_2","id_1","id_3"),
#' date = as.Date(c("2024-05-01","2024-08-01","2024-02-14","2024-05-01","2024-05-01","2024-01-01")),
#' participantCode = c("NSF REU", "CAHSI", "DREU","DREU", "CAHSI", "NSF REU"))
#' distinct_example <- distinct_example[order(distinct_example$id, distinct_example$date),]
#' distinct_rows(df = distinct_example, column = id, retain = first, keep_allVars = TRUE)
#'
#' # Example with multiple key columns and sorting
#' # Here the latest date record by wave per id is retained
#' distinct_example2 <- data.frame(id = c("cerpid_001","cerpid_002","cerpid_003","cerpid_003",
#' "cerpid_001","cerpid_001","cerpid_002","cerpid_003"),wave_flag = c("pre-survey", "pre-survey",
#' "post-survey","post-survey", "post-survey", "post-survey","post-survey","pre-survey"),
#' date = as.Date(c("2024-05-25","2024-05-01","2024-08-01","2024-03-20","2024-06-08","2024-06-03",
#' "2024-08-12","2024-02-14")))
#' distinct_example2 <- distinct_example2[order(distinct_example2$id, distinct_example2$date),]
#' distinct_example2R <- distinct_rows(df = distinct_example2, column = id, retain = last,
#' keep_allVars = TRUE, column2 = wave_flag)
#'
#' @export
distinct_rows <- function(df, column, retain, keep_allVars, ...){

  ### quick check on required parameters
  stopifnot("\nThe data set you supplied is not a tibble or data frame." = any(class(df) %in% c("tbl_df","tbl","data.frame")))

  ## set up
  originalVars <- names(df)
  df$order <- seq_len(nrow(df))
  column <- as.character(substitute(column))
  retain <- as.character(substitute(retain))
  keep_allVars <- grepl("t|true|y|yes|1", tolower(as.character(substitute(keep_allVars))))

  ## extract other specified arguments & set defaults
  dots <- as.list(substitute(...()))
  # set add_cols
  add_cols <- if (length(grep("^column", tolower(names(dots)))) > 0) {
    unlist(paste0(dots[grep("^column", tolower(names(dots)))]),use.names = FALSE) }
  else {
    NULL
  }

  ## all columns
  add_cols <- c(column, add_cols)

  ## create key column
  df[["distinct_key"]] <-
    if(length(add_cols) > 1){
      concatenate_cols(df = df, vars = add_cols)
    } else{
      df[[add_cols]]
    }

  ## order by key
  df <- df[order(df$distinct_key, decreasing = FALSE),]

  ## unique_count
  df[["unique_count"]] <- countUnique(df$distinct_key)

  ## order by unique count
  df <- df[order(df$unique_count, decreasing = ifelse(tolower(retain) == "first", FALSE, TRUE)),]

  ## identify duplicated rows
  df[["distinct"]] <- duplicated(df[["distinct_key"]])

  ## remove duplicated rows
  df <- df[df[["distinct"]] == 0, ]

  ## order by original order
  df <- df[order(df[["order"]], decreasing = FALSE),]

  ## retain all variables Y/N
  df <-
    if (keep_allVars | grepl("t|true|y|yes", tolower(as.character(keep_allVars)))) {
    df[originalVars]
  } else {
    df[column]
  }

  ## reset row names
  rownames(df) <- NULL

  ## return data frame
  df

}

## helper function to count unique
countUnique <- function(vectorValues){
  unlist(lapply(unique(vectorValues), \(x){seq_along(vectorValues[vectorValues == x])}))
}
