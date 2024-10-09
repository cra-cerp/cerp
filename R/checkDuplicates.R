#' Identify duplicates in a column and tag with each matched element's row number.
#'
#' @description This function can be used to identify and tag duplicates in a
#' given tibble or data frame column.
#'
#' @param dataSet A tibble or data frame object.
#' @param columnName A character vector containing the name of the column to search for
#' duplicates. NOTE: the column names can be passed with or without quotes, so long as
#' there are not special characters in the column name.
#' @param \dots additional parameters to pass to function. They include:
#' \itemize{
#'  \item \code{splitDelim}: A character string that will separate supplied elements into parts to
#'  identify potential matches within each each delimiter separated string. Default is NULL and the
#'  string will not be split.
#' }
#'
#' @returns A character vector. If the same value is identified more than once in the same column,
#' the corresponding row number/index for each matched instance is returned.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Simple searching for duplicate letters
#' letterData <- data.frame(letter = sample(letters[1:15], size = 10, replace = TRUE))
#' letterData$duplicates <- checkDuplicates(dataSet = letterData, columnName = "letter")
#' letterData
#'
#' # An example where one row has multiple elements to check duplicates for
#' colorData <- data.frame(colors = c("PurPle,Green", "blue", "Green", "blue",
#' "RED","purple"))
#' colorData$duplicates <- checkDuplicates(dataSet = colorData,
#' columnName = "colors", splitDelim = ",")
#' colorData
#'
#' @export
checkDuplicates <- function(dataSet, columnName, ...){

### quick check on required parameters
stopifnot("\nThe data set you supplied is not a tibble or data frame." = any(class(dataSet) %in% c("tbl_df","tbl","data.frame")),
          "\nThe column name you specified is not in the supplied data set." = columnName %in% names(dataSet))

### proceed otherwise
## warning (coerce to data.frame)
if(any(class(dataSet) %in% c("tbl_df","tbl"))){
	warning("Coercing dataSet to data.frame.", immediate = TRUE)
	dataSet <- as.data.frame(dataSet)
}

## set row id numbers
dataSet$rowID <- seq_len(nrow(dataSet))

## extract other specified arguments & set defaults
dots <- list(...)
# set splitDelim
splitDelim <- if (!is.null(dots[["splitDelim"]])) dots[["splitDelim"]] else NULL

## iterate over rows in data set and find duplicates
unlist(lapply(seq_len(nrow(dataSet)), \(elementNo){

	currentRowNum <- dataSet[elementNo,"rowID"]
	currentRow <- dataSet[[columnName]][elementNo]
	if (!is.null(splitDelim)) {currentRow <- trimws(unlist(strsplit(currentRow, splitDelim)))}
	otherRowNum <- dataSet[-elementNo,"rowID"]
	allOtherRows  <- dataSet[[columnName]][-elementNo]

	findDuplicate <- lapply(currentRow, \(x) grepl(paste0("\\b",tolower(x),"\\b"), tolower(allOtherRows)))

	findDuplicate  <- as.data.frame(findDuplicate)
	findDuplicate$rowNum <- otherRowNum
	duplicateRowNums <- findDuplicate[["rowNum"]][unique(unlist(lapply(findDuplicate, grep, pattern = TRUE)))]

	paste0(sort(as.numeric(c(currentRowNum, duplicateRowNums))), collapse = ",")

}))

}
