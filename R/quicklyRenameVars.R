#' This function can be used to quickly rename variables
#'
#' @description
#' This function allows the user to rename variables in a dataset. It requires a dataset and codebook
#' with two columns 1) one with old names, and 2) one with new names. NOTE: if the current variable name
#' is not found in the values supplied to "oldNameCol", the variable name will not be updated.
#'
#' @param dataSet A tibble or data frame.
#' @param codeBook A tibble or data frame with at least two columns: one for old names and one for new names.
#' @param currentVar A character vector containing the names of the variables to update.
#' @param oldNameCol The name of the column in the code book containing old names to be replaced.
#' @param newNameCol The name of the column in the code book containing new names.
#'
#' @returns A (character) vector of updated names.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' quicklyRename_dat <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 5))
#' quicklyRename_codebook <- data.frame(oldNames = paste0("V", 1:4),
#' newNames = paste0("question_",1:4))
#' names(quicklyRename_dat) <- quicklyRenameVars(dataSet = quicklyRename_dat,
#' codeBook = quicklyRename_codebook, currentVar = names(quicklyRename_dat),
#' oldNameCol = "oldNames", newNameCol = "newNames")
#'
#' @export
quicklyRenameVars <- function(dataSet, codeBook, currentVar, oldNameCol, newNameCol){

### quick check for list of data frames/tibbles
stopifnot("\nAt least one of the tables supplied is not a tibble or data frame."
            = all(vapply(list(dataSet, codeBook), \(x) any(class(x) %in% c("tbl_df","tbl","data.frame")), logical(1))))

### check that oldNameCol and newNameCol exist in codeBook
stopifnot("value supplied to `oldNameCol` does not exists in codeBook" = oldNameCol %in% names(codeBook),
          "value supplied to `newNameCol` does not exists in codeBook" = newNameCol %in% names(codeBook)
          )


### find new names
# create a lookup vector
oldNames <- codeBook[[oldNameCol]]
newNames <- codeBook[[newNameCol]]

# match the current variables with the old names in codeBook
matchedIndex <- match(currentVar, oldNames)

# if not match is found (NA) return original name; otherwise return new name
renamedVars <- ifelse(is.na(matchedIndex), currentVar, newNames[matchedIndex])

# return new names
renamedVars
}
