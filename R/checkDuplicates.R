#' Identify duplicates in a column and tag with each matched element's row number.
#'
#' @description This function can be used to identify and tag duplicates in a
#' vector, tibble, or data frame. Note: logical vectors  and lists are currently
#' not supported.
#'
#' @include zzz.R
#'
#' @importFrom S7 new_generic
#' @importFrom S7 S7_dispatch
#' @importFrom S7 method
#' @importFrom S7 class_data.frame
#' @importFrom S7 class_list
#' @importFrom S7 class_vector
#' @importFrom S7 class_logical
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom foreach "%dopar%"
#' @importFrom foreach foreach
#' @importFrom parallel stopCluster
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#'
#' @param x A vector or tibble/data frame object.
#' @param splitDelim A character string that will separate supplied elements into parts to
#'  identify potential matches within each each delimiter separated string. Default is NULL and the
#'  string will not be split.
#'
#' @param \dots additional parameters to pass to function. They include:
#' \itemize{
#'  \item \code{vars}: A character vector of column names. Only used when a data.frame/tibble
#' is passed. Default is NULL and all columns will be searched for duplicates.
#' }
#'
#' @returns A character vector if a vector is supplied. Columns that returned are of type character.
#' If the same value is identified more than once in the same column, the corresponding row number/
#' index for each matched instance is returned.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Simple searching for duplicate letters
#' letterData <- data.frame(letter = sample(letters[1:15], size = 10, replace = TRUE))
#' letterData$duplicates <- checkDuplicates(x = letterData$letter)
#' letterData
#'
#' # An example using splitDelim
#' colorData <- data.frame(colors = c("PurPle,Green", "blue", "Green", "blue",
#' "RED","purple"))
#' colorData |> dplyr::mutate(colorData_dupes = checkDuplicates(x = colorData$colors,
#' splitDelim = ","))
#'
#' @export
checkDuplicates <- S7::new_generic("checkDuplicates", "x", function(x, ...) {
  S7::S7_dispatch()
})

#' @export
S7::method(checkDuplicates, S7::class_vector) <-
  function(x, ..., splitDelim = NULL, parallel = FALSE) {
  ### check vector length
  stopifnot("The vector you supplied is of length zero" = length(x) > 0)

  ### pre-steps
  # convert all elements to lowercase for case-insensitive comparison
  lowerVec <- tolower(as.character(x))
  # pre-allocate a character vector to store the duplicates
  duplicates <- vector("character", length = length(x))
  # element indices
  indices <- seq_along(x)

  ## use foreach/dopar if parallel is specified
  if(parallel) {
    ## make cluster + register the parallel backend
    cl <- parallel::makeCluster(parallel::detectCores() - 1, type = "FORK")
    doParallel::registerDoParallel(cl)

    duplicates <-
      foreach::foreach(i = indices, .packages=c("foreach","parallel"), .combine = c) %dopar% {
        # find duplicate rows
        findDuplicate <- findDuplicates(lowerVec[i], lowerVec[-i], splitDelim = splitDelim)
        # return a sorted, comma-separated list of row numbers
        paste0(sort(c(i, indices[-i][which(as.logical(findDuplicate))])), collapse = ",")
      }

    # shut the cluster down
    parallel::stopCluster(cl)

  ## use regular do if do is specified
  } else {
    ### iterate through the vector and find duplicates
    for (i in indices) {
      # Find the duplicate indices by comparing current element with all other elements
      findDuplicate <- findDuplicates(lowerVec[i], lowerVec[-i], splitDelim = splitDelim)
      # Store the indices of the duplicates
      duplicates[i] <- paste0(sort(c(i, indices[-i][which(as.logical(findDuplicate))])), collapse = ",")
    }
  }

  ### return vector of duplicate indices
  duplicates
}

#' @export
S7::method(checkDuplicates, S7::class_data.frame) <-
  function(x, vars = NULL, ..., splitDelim = NULL, parallel = FALSE) {
  ### apply method (if vars specified, apply to specified cols; otherwise apply to all cols)
  if (is.null(vars)) {
    vars <- names(x)
  } else {
    ## check that all vars exist in data frame
    if (!all(vars %in% names(x))) {
      stop(paste0("\nThe following variables do not exist in the supplied data.frame:\n",
                  paste0(vars[match(vars, names(x), nomatch = 0L) == 0L], collapse = "\n")))
    }
  }

  ### proceed to method
  ## find duplicates
  data_frame <- purrr::map(.x = x[vars], .f = checkDuplicates, splitDelim = splitDelim, ...)

  ## rename columns in case they will be merged with another data frame/tibble
  names(data_frame) <- paste0(names(data_frame), "_duplicates")

  ### return data frame/tibble
  data_frame |> dplyr::bind_rows()
  }

#' @export
S7::method(checkDuplicates, S7::class_Date) <- function(x, ...) {
  x <- as.character(x)
  checkDuplicates(x, ...)
}

#' @export
S7::method(checkDuplicates, S7::class_list) <- function(x, ...) {
  stop("Lists are currently not supported.")
}

#' @export
S7::method(checkDuplicates, S7::class_logical) <- function(x, ...) {
  stop("Logical vectors are currently not supported.")
}

#' @export
S7::method(checkDuplicates, S7::class_complex) <- function(x, ...) {
  stop("Complex vectors are currently not supported.")
}

### helper function to check for duplicates in other elements
findDuplicates <- function(currentElement, allOtherElements, splitDelim = splitDelim) {
  if (!is.null(splitDelim)) {
    currentElement <- trimws(unlist(strsplit(currentElement, splitDelim)))
  }
  +grepl(paste0("\\b", tolower(currentElement), "\\b", collapse = "|"), allOtherElements)
}
