#' A function for extracting matched string from a vector.
#'
#' @description
#' This function is similar to str_extract from stringr. NOTE: case is ignored when matches are sought.
#'
#' @param object a vector to search for match string
#' @param objectDictionary character vector to match in object
#' @param \dots Additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{objectEscape}: An optional parameter that when set to TRUE escapes all punctuation in the object
#'  to search. Default is FALSE.
#'  \item \code{objectDictEscape}: An optional parameter that when set to TRUE escapes all punctuation in the
#'  object dictionary to search. Default is FALSE.
#'  \item \code{specialRun}: An optional argument that when set to TRUE will return all matched string
#'  separated by a semi-colon (e.g., match1; match2; match3). The default is FALSE, as only one match
#'  is typically expected.
#'  \item \code{invert}: An optional argument that when set to TRUE will return values for elements that
#'  do not match.
#'  }
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # An example using text
#' string_to_match <- c("Choose up to 2 responses.", "Select all that apply.")
#' string_to_search <- data.frame(orig_varLabel = c("After you complete your current program,
#' what are your plans after graduation? Choose up to 2 responses.",
#' "How did you learn about the REU? Select all that apply.",
#' "This should return NA"))
#' string_to_search$extracted_string <- str_match_extract(object = string_to_search$orig_varLabel,
#' objectDictionary = string_to_match)
#'
#' # An example returning multiple matched string
#' this_match <- c("Choose up to 2 responses.", "Select all that apply.", "Select all that apply")
#' string_to_search2 <- data.frame(orig_varLabel = c("After you complete your current program,
#' what are your plans after graduation? Choose up to 2 responses. Select all that apply",
#' "How did you learn about the REU? Select all that apply.", "This should return NA"))
#' string_to_search2$extracted_string <- str_match_extract(object = string_to_search2$orig_varLabel,
#' objectDictionary = this_match, specialRun = TRUE)
#'
#' @export
str_match_extract <- function(object, objectDictionary, ...){

### quick check on required parameters
stopifnot("\nThe object you would like to check for string matches is not of type vector." = is.vector(object),
          "\nThe objectDictionary you would like to use to extract matched string from is not of type vector."
          = is.vector(objectDictionary))

### otherwise, proceed
## extract other specified arguments & set defaults
dots <- list(...)
# set objectEscape
objectEscape <-
  if (!is.null(dots[["objectEscape"]])) {
    dots[["objectEscape"]] }
  else {
    FALSE
  }
# set objectDictEscape
objectDictEscape <-
  if (!is.null(dots[["objectDictEscape"]])) {
    dots[["objectDictEscape"]] }
  else {
    FALSE
  }
# set specialRun
specialRun <-
  if (!is.null(dots[["specialRun"]])) {
    dots[["specialRun"]] }
  else {
    FALSE
  }
# set invert
invert <-
  if (!is.null(dots[["invert"]])) {
    dots[["invert"]] }
  else {
    FALSE
  }

## QUESTIONING: clean object dictionary (only for survey question cases)
objectDictionary <- gsub("\\$.*\\{.*\\}.*", "", objectDictionary)

## escape punctuations
if (objectEscape){object <- escape_punct(object)}
if (objectDictEscape){objectDictionary <- escape_punct(objectDictionary)}


### define parameters
## combine objectDictionary as a single pattern
pattern <- paste0(tolower(objectDictionary), collapse = "|")

### begin find/match and extract
result <- vapply(object, extract_matches, pattern = pattern, invert = invert,
                 specialRun = specialRun, FUN.VALUE = character(1),
                 USE.NAMES = FALSE)
# return result
return(result)

}

## helper function to process each element
extract_matches <- function(current_object, pattern, invert, specialRun) {
  # perform match search
  match_found <- regmatches(current_object, gregexpr(pattern = pattern,
                                                     ignore.case = TRUE,
                                                     text = current_object),
                                                     invert = invert)
  # return matches found
  if(length(match_found[[1]]) == 0) {
    # return NA if no match found
    return(NA_character_)
  } else if (specialRun) {
    # special handling for matches when specialRun is TRUE
    toReturn <- match_found[[1]][match_found[[1]] != ""]
    return(paste(toReturn, collapse = ";"))
  } else {
    # normal match return
    return(match_found[[1]][match_found[[1]] != ""])
  }

}

