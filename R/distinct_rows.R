#' A function to retain/keep unique rows
#'
#' @description This function is similar to dplyr's distinct() function and can be used to
#' retain distinct/unique rows.
#'
#' @param dataset A tibble or data frame.
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
#' distinct_rows(dataset = distinct_example, column = id, retain = first, keep_allVars = TRUE)
#'
#' # Example with multiple key columns and sorting
#' # Here the latest date record by wave per id is retained
#' distinct_example2 <- data.frame(id = c("cerpid_001","cerpid_002","cerpid_003","cerpid_003",
#' "cerpid_001","cerpid_001","cerpid_002","cerpid_003"),wave_flag = c("pre-survey", "pre-survey",
#' "post-survey","post-survey", "post-survey", "post-survey","post-survey","pre-survey"),
#' date = as.Date(c("2024-05-25","2024-05-01","2024-08-01","2024-03-20","2024-06-08","2024-06-03",
#' "2024-08-12","2024-02-14")))
#' distinct_example2 <- distinct_example2[order(distinct_example2$id, distinct_example2$date),]
#' distinct_example2R <- distinct_rows(dataset = distinct_example2, column = id, retain = last,
#' keep_allVars = TRUE, column2 = wave_flag)
#'
#' @export
distinct_rows <- function(dataset, column, retain, keep_allVars, ...){

### quick check on required parameters
stopifnot("\nThe data set you supplied is not a tibble or data frame." =
            (sum(grepl("tbl_df|tbl|data.frame", class(dataset))) > 0))

### otherwise, proceed
## create count unique function
countUnique <- function(vectorValues){

uniqueVals <- lapply(unique(vectorValues), FUN = function(x){
  1:length(vectorValues[vectorValues == x])
  })
unlist(uniqueVals)

}

## set up
dataset <- dataset
originalVars <- names(dataset)
dataset$order <- 1:nrow(dataset)
column <- as.character(substitute(column))
column <- paste0(column)
retain <- as.character(substitute(retain))
retain <- paste0(retain)
keep_allVars <- as.character(substitute(keep_allVars))
keep_allVars <- paste0(keep_allVars)
keep_allVars <- ifelse(grepl("t|true|y|yes|1", tolower(keep_allVars)), TRUE, FALSE)

## check for dots_list
dots_list <- as.list(substitute(...()))
if(length(dots_list) !=0){
	dots <- unlist(dots_list)
    add_cols <- dots[grep("^column", names(dots))]
    add_cols <- paste0(add_cols)
} else{
  dots <- NULL
}

## set default input values
add_cols <- if(is.null(dots) | is.null(dots[grep("^column", names(dots))])){NULL} else{add_cols}

## all columns
add_cols <- c(column, add_cols)

## create key column
dataset[["distinct_key"]] <- if(length(add_cols) > 1){
	apply(dataset[,add_cols], 1, paste, collapse = "")
} else{
	dataset[[add_cols]]
}

## order by key
dataset <- dataset[order(dataset$distinct_key, decreasing = FALSE),]

## unique_count
dataset[["unique_count"]] <- countUnique(dataset$distinct_key)

## order by unique count
dataset <- dataset[order(dataset$unique_count, decreasing = ifelse(tolower(retain) == "first", FALSE, TRUE)),]

## identify duplicated rows
dataset[["distinct"]] <- duplicated(dataset[["distinct_key"]])

## remove duplicated rows
dataset <- dataset[dataset$distinct == 0, ]

## order by original order
dataset <- dataset[order(dataset$order, decreasing = FALSE),]

## retain all variables Y/N
if(isTRUE(keep_allVars) | grepl("t|true|y|yes", tolower(as.character(keep_allVars)))){
	dataset <- subset(dataset, select = originalVars)
} else{
	dataset <- subset(dataset, select = column)
}

## reset row names
rownames(dataset) <- NULL

## return data frame
dataset

}
