#' Function for strategically merging an auxiliary table with a key table table.
#'
#' @description
#' This function strategically merges two tables: 1) a key table and 2) an auxiliary table.
#' NOTE: The default key variable to merge on is "Variable". You must supply a character vector
#' of variables to lookup that exist in both the key and auxiliary tables. Key table data are
#' prioritized. In the event that a field/column is NA the column will be dropped. This function
#' was originally developed to be able to add data to an existing table that has NA's in certain
#' columns. If you accidentally add the same field/column with the same name and DIFFERENT values
#' to either table, a new row/observation will be added to the key table.
#'
#' @param fullTabl A tibble or data frame; key table.
#' @param auxTabl A tibble or data frame; auxiliary table.
#' @param vars_to_add a character vector containing the names of the variables to look up.
#' @param \dots Additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{set_commonkey}: An optional parameter you can specify. In the event the key variable in one
#'  table has a different name from the key variable in the second table, set this input to TRUE and then use
#'  the common_key input to set a new common key variable name.
#'  \item \code{fullTabl_variableCol}: An optional parameter (character vector) you can specify. In the event
#'  the key variable in the key table has a different name from the key variable in the auxiliary table,
#'  you must supply the field/variable name of the key variable to merge on.
#'  \item \code{auxTabl_variableCol}: An optional parameter (character vector) you can specify. In the event
#'  the key variable in the auxiliary table has a different name from the key variable in the key table,
#'  you must supply the field/variable name of the key variable to merge on.
#'   \item \code{common_key}: An optional parameter (character vector) you can specify. This will set
#'   a new common key variable name.
#'  }
#'
#' @returns A data frame.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' full_tabl <- data.frame(orig = "aux_disability_grouped", timeFlag = "globalVar", Values = NA,
#' Labels = NA, string = NA, selectType = NA, orig_varLabel = NA)
#' aux_tabl <- data.frame(var_name = "aux_disability_grouped", Values = "0,1",
#' questionStem = "Do you have any disabilities?",
#' Labels = "No disabilities;One or more disabilities", selectType = "single choice",
#' groupType = "REU")
#' mergeNamesTabl(fullTabl = full_tabl, auxTabl = aux_tabl, vars_to_add = "aux_disability_grouped",
#' fullTabl_variableCol = "orig", auxTabl_variableCol = "var_name", set_commonkey = TRUE,
#' common_key = "varNAME")
#'
#' @export
mergeNamesTabl <- function(fullTabl, auxTabl, vars_to_add, ...){

### quick check key
stopifnot("\nAt least one of the tables supplied is not a tibble or data frame."
          = all(vapply(list(fullTabl,auxTabl), \(x) any(class(x) %in% c("tbl_df","tbl","data.frame")), logical(1))))

### otherwise, proceed
## extract other specified arguments (these are optional)
dots <- list(...)
# set fullTabl_variableCol
fullTabl_variableCol <- if (!is.null(dots[["fullTabl_variableCol"]])) dots[["fullTabl_variableCol"]] else "Variable"
# set auxTabl_variableCol
auxTabl_variableCol <- if (!is.null(dots[["auxTabl_variableCol"]])) dots[["auxTabl_variableCol"]] else "Variable"
# set set_commonkey
set_commonkey <- if (!is.null(dots[["set_commonkey"]])) as.logical(dots[["set_commonkey"]]) else FALSE
# set common_key
common_key <- if (!is.null(dots[["common_key"]])) dots[["common_key"]] else "Variable"

# rename common key if set
if(set_commonkey){
  names(fullTabl)[grep(fullTabl_variableCol, names(fullTabl))] <- common_key
  names(auxTabl)[grep(auxTabl_variableCol, names(auxTabl))] <- common_key
  fullTabl_variableCol <- common_key
  auxTabl_variableCol <- common_key
}

# find unique variables
unique_vars <- vars_to_add

# iterate over unique variables fed
full_tabl_final <-
  lapply(unique_vars, function(x){
    if(all(any(grepl(x, fullTabl)),any(grepl(x, auxTabl)))){
        # subset by variable
        tabl1Sub <- fullTabl[fullTabl[[fullTabl_variableCol]] == x,]
        tabl2Sub <- auxTabl[auxTabl[[auxTabl_variableCol]] == x,]
        # remove columns where is missing
        tabl1Sub <- tabl1Sub[!apply(is.na(tabl1Sub), 2, any)]
        tabl2Sub <- tabl2Sub[!apply(is.na(tabl2Sub), 2, any)]
        # merge on all
        merge(tabl1Sub, tabl2Sub, all = TRUE, sort = FALSE)
    }
  })

# merge into one table
do.call(dplyr::bind_rows, full_tabl_final)
}
