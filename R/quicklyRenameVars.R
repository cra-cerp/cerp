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
stopifnot("\nThe list of data frames/tibbles you supplied has at least one object that is not a tibble or data frame."
          = (all(sapply(list(dataSet, codeBook), function(x) sum(grepl("tbl_df|tbl|data.frame", class(x))) > 0))))

### find new names by iterating over current variable names
newNames <- sapply(currentVar, function(currentName){
## find row in codebook with the existing name
currentRow <- codeBook[grepl(paste0("^", currentName,"$"), codeBook[[oldNameCol]]),]
## return new name if it exists otherwise keep current name
if(length(currentRow[[newNameCol]]) == 0){
	currentName
} else{
  ## otherwise, do nothing
	currentRow[[newNameCol]]
   }
})
# unlist new names
unlist(newNames)

}
