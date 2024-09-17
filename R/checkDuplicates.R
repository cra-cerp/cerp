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
stopifnot("\nThe data set you supplied is not a tibble or data frame." = (sum(grepl("tbl_df|tbl|data.frame", class(dataSet))) > 0),
          "\nThe column name you specified is not in the supplied data set." = (sum(grepl(paste0("^",columnName,"$"), names(dataSet))) > 0))

### proceed otherwise
## set row id numbers
dataSet$rowID <- 1:nrow(dataSet)

## extract other specified arguments (these are optional)
dots_list <- list(...)
if(length(dots_list) != 0){
  dots <- unlist(dots_list)
	splitDelim <- dots[grepl("splitdelim", tolower(names(dots)))]
} else{
  dots <- NULL
}

## otherwise set these inputs to null/pre-determined string
splitDelim <- if(is.null(dots) | !any(grepl("splitdelim", tolower(names(dots))))){NULL} else{splitDelim}

## iterate over rows in data set and find duplicates
sapply(seq_len(nrow(dataSet)), function(elementNo){

	currentRowNum <- dataSet[elementNo,"rowID"]
	currentRow <- dataSet[[columnName]][elementNo]
	currentRow <- if(is.null(splitDelim)){currentRow} else{trimws(unlist(strsplit(currentRow, splitDelim))) }
	otherRowNum <- dataSet[-elementNo,"rowID"]
	allOtherRows  <- dataSet[[columnName]][-elementNo]

	findDuplicate <- sapply(currentRow, function(x) grepl(paste0("\\b",tolower(x),"\\b"), tolower(allOtherRows)),
	simplify  = FALSE)

	findDuplicate  <- as.data.frame(findDuplicate)
	findDuplicate$rowNum <- otherRowNum
	duplicateRowNums <- findDuplicate[["rowNum"]][unique(unlist(sapply(findDuplicate, grep, pattern = TRUE)))]

	paste0(sort(as.numeric(c(currentRowNum, duplicateRowNums))), collapse = ",")

  })
}
