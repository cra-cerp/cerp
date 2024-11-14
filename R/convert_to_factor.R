#' Convert data.frame column to labelled factor.
#'
#' @description
#' This function leverages base's factor function to convert columns in a data.frame
#' or tibble to a labelled factor. NOTE: Defaults of addDelim are used for levels (,)
#' and labels (;).
#'
#' @param df a data.frame or tibble.
#' @param var a character vector containing variable/column names to convert to a factor.
#' @param codeBook a data.frame or tibble containing: variable names, variable values, and value labels.
#' @param \dots additional parameters to pass to function.These include: variableName_col (an optional input
#' you can specify; variable names will be extracted from this column/field) and variableValues_col (an optional
#' input you can specify; variable values will be extracted from this column/field).
#' \itemize{
#'  \item \code{variableName_col}: An optional argument. This parameter is used to specify the name of the
#'  column/field in the codeBook to extract variable names from.
#'  \item \code{variableValues_col}: An optional argument. This parameter is used to specify the name of the
#'  column/field in the codeBook to extract variable values from.
#'  \item \code{valueLabels_col}: An optional argument. This parameter is used to specify the name of the
#'  column/field in the codeBook to extract value labels from.
#'}
#'
#' @returns A data.frame or tibble.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' codeBook_race <- data.frame(variableName = "race_ethnicity", variableValues = "1,2,3,4",
#' valueLabels = "Black/African American;Asian/Asian American;Indigenous;Native Hawaiian/Pacific Islander")
#'
#' race_tabl <- data.frame(race_ethnicity = sample(c(1:4, NA), size = 500, replace = TRUE))
#'
#' convert_to_factor(df = race_tabl, var = "race_ethnicity", codeBook = codeBook_race)
#'
#' @export
convert_to_factor <- function(df, var, codeBook, ...) {

  ### check objects
  stopifnot("df is not of tibble or data.frame" =
              any(class(df) %in% c("tbl_df","tbl","data.frame")),
            "codeBook is not of tibble or data.frame" =
              any(class(codeBook) %in% c("tbl_df","tbl","data.frame")),
            "var is length of zero" = length(var) > 0)

  ### variable check
  if(any(!var %in% names(df))) {

    stop(paste0("\nThe following variables do not exist in the supplied data frame:\n",
                paste0(var[which(!var %in% names(df))],collapse = "\n")))
  }

  ### extract dots
  dots <- list(...)
  ## set defaults
  # variable name column
  variableName_col <-
    if (!is.null(dots[["variableName_col"]])) {
      dots[["variableName_col"]]
    } else {
      "variableName"
    }
  # variable values column
  variableValues_col <-
    if (!is.null(dots[["variableValues_col"]])) {
      dots[["variableValues_col"]]
    } else {
      "variableValues"
    }
  # value labels column
  valueLabels_col <-
    if (!is.null(dots[["valueLabels_col"]])) {
      dots[["valueLabels_col"]]
    } else {
      "valueLabels"
    }

  ### recode columns
  df[var] <- lapply(var, helper_convert_to_factor, df = df, codeBook = codeBook,
                    variableName_col = variableName_col,
                    variableValues_col = variableValues_col,
                    valueLabels_col = valueLabels_col)

  ### return df
  return(df)

}


### helper function
helper_convert_to_factor <- function(df, var, codeBook, ...) {

  ### extract dots
  dots <- list(...)
  ## set defaults
  # variable name column
  variableName_col <-
    if (!is.null(dots[["variableName_col"]])) {
      dots[["variableName_col"]]
    } else {
      "variableName"
    }
  # variable values column
  variableValues_col <-
    if (!is.null(dots[["variableValues_col"]])) {
      dots[["variableValues_col"]]
    } else {
      "variableValues"
    }
  # value labels column
  valueLabels_col <-
    if (!is.null(dots[["valueLabels_col"]])) {
      dots[["valueLabels_col"]]
    } else {
      "valueLabels"
    }

  ### extract levels and labels
  levels <- factor_levels(
    x = var,
    key = codeBook,
    variableName_col = variableName_col,
    variableValues_col = variableValues_col
  )
  labels <- factor_labels(
    x = var,
    key = codeBook,
    variableName_col = variableName_col,
    valueLabels_col = valueLabels_col
  )

  ### check objects
  stopifnot("vector in df is length of zero" = length(df[[var]]) > 0,
            "levels vector is length of zero" = length(levels) > 0,
            "labels vector is length of zero" = length(labels) > 0)

  ### recode vector
  factor(df[[var]], levels = levels, labels = labels)

}
