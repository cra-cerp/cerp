#' Function to remove HTML entities from string
#'
#' @description
#' This function removes HTML entities from string then standardizes spaces.
#'
#' @importFrom rvest html_text
#' @importFrom rvest read_html
#' @importFrom mgsub mgsub
#' @importFrom stringr str_squish
#'
#' @param x a vector to remove HTML entities from
#'
#' @author Ama Nyame-Mensah
#'
#' @export
htmlParser <- function(x){

### quick check on required parameters
stopifnot("\nThe object you would like to remove html tags/entities from is not of type vector." =
            is.vector(x) || is.character(x))

### otherwise, proceed
## Step 1: replace html entities and unwanted characters using mgsub
cleaned_x <- mgsub::mgsub(x,
                          pattern = c("<[^>]+>", # remove html tags
                                      "  ", # replace double spaces
                                      "&rdquo;","&ldquo;","&amp;",
                                      "&rsquo;","&lsquo;","&#39;",
                                      "[\u201a\u00c4\u00f4\u2018\u2019\u055A\u201B\uFF07]",
                                      "&quot;","[^ -~]+"), # remove non-ascii characters
                  replacement = c("" , " ","'","'","'","'","'","'","'","'"," "))

## Step 2: remove leading/trailing white space
cleaned_x <- trimws(cleaned_x)

## Step 3: remove remaining html using rvest (if applicable)
cleaned_x <- tryCatch({
  rvest::html_text(rvest::read_html(cleaned_x))},
  error = function(e){
    cleaned_x # in-case of error return previously cleaned text
    })

## Step 4: remove extra spaces and return final result
stringr::str_squish(cleaned_x)

}
