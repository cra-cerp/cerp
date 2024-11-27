#' General function for creating pushed/global vector of values
#'
#' @description This function can be used to find and retain the most recent
#' response for a record.
#'
#' @importFrom mgsub mgsub
#' @importFrom tidytable as_tidytable
#' @importFrom purrr map_chr
#'
#' @param df A tibble or data frame object.
#' @param vars A character vector of unique variable (name) stems.
#' @param \dots additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{groupFlag}: An optional parameter that is a character vector containing the group
#'  or time flag pattern to identify each variable stem. The default is "_w". This default can be
#'  overridden by specifying another group or time flag pattern. See example 2 below.
#'  }
#'
#' @author Ama Nyame-Mensah
#'
#' @returns A character vector that is column-binded to the original supplied df. NOTE: The most recent
#' (i.e., LATEST) non-null, non-missing value of the LATEST group/time flag is retained.
#'
#' @examples
#' # Example with default group/time flag
#' sexBirth_data
#' createGlobalVars(df = sexBirth_data, vars = "sex")
#'
#' # Example with user-specified group/time flag (note the time flag pattern is "_t")
#' yearBorn_data
#' createGlobalVars(df = yearBorn_data, vars = "yearBorn", groupFlag = "_t")
#'
#' @export
# Function to create global variables
createGlobalVars <- function(df, vars, ..., groupFlag = "_w\\d$") {

	# convert select columns to character
	colNames <- grep(paste0("^", vars, groupFlag, collapse = "|"), names(df), value = TRUE)
	df[colNames] <- lapply(df[colNames], as.character)
	# questioning: purrr::map_at(.x = df, .at = colNames, .f = as.character) |> dplyr::bind_cols()

	# tidy table
	if (!inherits(df, "tidytable")) {
		df <- tidytable::as_tidytable(df)
		}


	# Main manipulation
	new_GlobalVars <- lapply(vars, \(stemName) checkUpdate(df, stemName, groupFlag))

	# Check + stop if any names already exist
	existing_names <- intersect(names(df), vars)
	if(length(existing_names) > 0) {
		stop(paste0("\nThe following variables already exist in the supplied data set:\n",
		paste(existing_names, collapse = "\n")))
	}

	# Add new variables to data.table
	for (i in seq_along(vars)) {
		df[,vars[i]] <- new_GlobalVars[[i]]
	}

	# convert back to data.frame
	class(df) <- class(as.data.frame(df))

	# return df
	df
}


# Helper function for finding most up-to-date value to be pushed
newVarValues <- function(df, rowNum, stemName, groupFlag = "_w\\d$") {

  # Set up the groupFlag if not correct
  groupFlag <- ifelse(grepl(escape_punct("\\d$"), groupFlag), groupFlag, paste0(groupFlag, "\\d$"))
  groupFlag <- ifelse(grepl(pattern = "^_", groupFlag), groupFlag, paste0("_",groupFlag))
  groupFlagShortened <- ifelse(grepl("^_w",groupFlag), "_w", gsub("\\d$","", groupFlag, fixed = TRUE))

  # Pull current row of data
  currentRow <- df[rowNum,,drop = FALSE]
  currentRowUpdate <- unlist(Filter(function(df) !any(is.na(df)) & df != "", currentRow))

  # Get wave information for variables
  maxWave <- gsub(paste0("^", stemName, groupFlagShortened), "", names(currentRowUpdate))
  maxWave <- ifelse(length(maxWave) == 0, NA, max(maxWave))

  # Find values
  if (length(unique(currentRowUpdate)) == 1) {
    return(currentRowUpdate[[1]])
  } else if (length(unique(currentRowUpdate)) > 1) {
    return(currentRowUpdate[[paste0(stemName, groupFlagShortened, maxWave)]])
  } else {
    return(NA)
  }
}


# Helper function for creating a pushed/global vector of values
checkUpdate <- function(df, stemName, groupFlag = groupFlag) {
  # Get the column names related to this stem
  columnNames <- grep(paste0("^", stemName, groupFlag, collapse = "|"), names(df), value = TRUE)

  # subset df
  df <- df[, columnNames, with = FALSE]

  # Retrieve new values for each row
  purrr::map_chr(seq_len(nrow(df)), \(rowNum) newVarValues(df, rowNum, stemName, groupFlag))
}
