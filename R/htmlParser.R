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
stopifnot("\nThe object you would like to remove html tags/entities from is not of type vector." = is.vector(x))

### otherwise, proceed
## replace characters
x <- mgsub::mgsub(x, pattern = c("<[^>]+>","  ","&rdquo;","&ldquo;","&amp;",
                                 "&rsquo;","&lsquo;","&#39;","\\u201a\\u00c4\\u00f4",
                                 "&quot;","[^ -~]+"),
                  replacement = c("" , " ","'","'","'","'","'","'","'","'"," "))

## double check on the blank spaces
newX <- trimws(x, which = "both")

## strip remaining html
newX2 <- tryCatch({rvest::html_text(rvest::read_html(newX))},
                  error = function(e){ return(newX)})

## str_squish (return cleaned text)
stringr::str_squish(newX2)

}
