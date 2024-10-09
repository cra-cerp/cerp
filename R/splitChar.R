#' Split a sting on character (delimeter)
#'
#' @description
#' This is a generalized version of strsplit. You can retain 1 to n elements.
#'
#' @param vector A vector to split.
#' @param split A delimiter to split string (e.g., comma, semi-colon).
#' @param elementToKeep the number of elements to retain.
#' @param \dots Additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{addDelim}: An optional parameter you can specify when retaining more than one element.
#'  The default value is "".
#'  }
#'
#' @returns A vector of values.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example retaining the first element
#' city_data <- data.frame(city_state = c("Baltimore, Maryland", "Raleigh, North Carolina",
#' "Philadelphia, Pennsylvania"))
#' city_data$City <- splitChar(vector = city_data$city_state, split = ",", elementToKeep = 1)
#'
#' # Example retaining the first and third elements (i.e., first and last names)
#' name_data <- data.frame(full_name = c("Linda E. Evans", "Akua Taylor Offor", "Heather M. Lutz"))
#' name_data$first_middle <- splitChar(vector = name_data$full_name,
#' split = " ", elementToKeep = c(1,3), addDelim = " ")
#'
#' @export
splitChar <- function(vector, split, elementToKeep, ...){

### quick check for vector
stopifnot("\nThe vector you supplied is not of type vector." = is.vector(vector),
          "\nAt least one of the values supplied to elementToKeep is not a whole number" =
            all(vapply(elementToKeep, \(x) x %% 1 == 0,logical(1))))

### otherwise, proceed
## extract other specified arguments & set defaults
dots <- list(...)
# set addDelim
addDelim <-
  if (!is.null(dots[["addDelim"]])) {
    dots[["addDelim"]] }
  else {
    ""
  }

### main manipulation
## iterate over vector
unlist(lapply(vector, function(currentVec){
  # unlist and split + keep select elements
  splitStr <- unlist(strsplit(currentVec, split))[elementToKeep]
  splitStr <- trimws(splitStr)
  splitStr <- splitStr[!is.na(splitStr)]
  # to return
  paste0(splitStr, collapse = addDelim)
  })
  )
}

