#' Function for quickly generating SPSS labelled vector using haven
#'
#' @description This function can be used to quickly generate an SPSS labelled vector using
#' labelled_spss from the haven package. Users can either provide codebook inputs OR specify
#' inputs, but not both.
#'
#' @importFrom haven labelled_spss
#' @importFrom rlang parse_expr
#' @importFrom stats setNames
#'
#' @param dataSet a tibble or data frame.
#' @param variableName name of a variable in a tibble or data frame to generate SPSS labelled vector for.
#' @param \dots additional parameters to pass to function. These include:
#' \itemize{
#'  \item \code{codeBook}: The name (string) of a tibble or data frame that contains codebook
#'  information.
#'  \item \code{codeBook_variableCol}: The name of the field/column in the codebook that contains
#'  variable names.
#'  \item \code{codeBook_questionLabelCol}: The name of the field/column in the codebook that contains
#'  question labels.
#'  \item \code{codeBook_valuesCol}: The name of the field/column in the codebook that contains variable
#'  values. Variable values should be in ONE cell and concatenated together using a comma (e.g., "1,2,3")).
#'  \item \code{codeBook_valueLabelsCol}: The name of the field/column in the codebook that contains value
#'  label. Value labels should be in ONE cell and concatenated together using a semi-colon (e.g., "One;Two;Three")).
#'  \item \code{variableValues}: A user-supplied vector of (variable) values. Variable values should be
#'  concatenated together using a comma (e.g., "1,2,3")).
#'  \item \code{valueLabels}: A user-supplied vector of value labels. Value labels should be concatenated
#'  together using a semi-colon (e.g., "One;Two;Three")).
#'  \item \code{questionLabel}: A user-supplied vector with a question label. Only one element should be
#'  supplied).
#'  \item \code{recodeMissing}: TRUE/FALSE. If set to TRUE, missing values are recoded. If set to FALSE
#'  values are not recoded.
#'  \item \code{recodeMissingValue} A user-supplied value to apply to missing values. If no missing value
#'  is supplied but recodeMissing is set to TRUE, then the default value "." is applied.
#' }
#'
#' @returns A vector of labelled values.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' # Example with supplied inputs, and missing is not recoded
#' spss_labelled_example <- data.frame(race_ethnicity = sample(c(1:4, NA),
#' size = 10, replace = TRUE))
#' create_spss_labelledVector_function(dataSet = spss_labelled_example,
#' variableName = "race_ethnicity",
#' variableValues = 1:4, valueLabels = c("Asian/Asian American", "Black/African American",
#' "Native Hawaiian/Pacific Islander", "Hispanic/Latinx"),
#' questionLabel = "What is your race/ethnicity?", recodeMissing = FALSE)
#'
#' # Example with supplied codebook inputs
#' codebook_spss_labelled <- data.frame(variable_name = "sexNow",
#' question_label = "Do you currently describe yourself as male, female, or transgender?",
#' values = "1,2,3,4", value_labels = "Male;Female;Transgender;None of these")
#' spss_labelled_example2 <- data.frame(sexNow = sample(c(1:4, NA),
#' size = 10, replace = TRUE))
#' create_spss_labelledVector_function(dataSet = spss_labelled_example2,
#' codeBook = "codebook_spss_labelled", variableName = "sexNow",
#' codeBook_variableCol = "variable_name", codeBook_questionLabelCol = "question_label",
#' codeBook_valuesCol = "values", codeBook_valueLabelsCol = "value_labels",
#' recodeMissing = TRUE, recodeMissingValue = -9999)
#'
#' @export
create_spss_labelledVector_function <- function(dataSet, variableName,...){

### quick check on required parameters
stopifnot("\nThe data set you supplied is not a tibble or data frame." = (sum(grepl("tbl_df|tbl|data.frame", class(dataSet))) > 0),
          "\nThe column name you specified is not in the supplied data set." = (sum(grepl(paste0("^",variableName,"$"), names(dataSet))) > 0))

### proceed otherwise
## extract other specified arguments
dots_list <- list(...)

if(length(dots_list) != 0 & any(grepl("^codebook", tolower(names(dots_list))))){
	dots <- unlist(dots_list)
    codeBook <- dots[grep("^codebook$", tolower(names(dots)))]
    codeBook_variableCol <- dots[grep("^codebook_variablecol$", tolower(names(dots)))]
    codeBook_questionLabelCol <- dots[grep("^codebook_questionlabelcol$", tolower(names(dots)))]
    codeBook_valuesCol <- dots[grep("^codebook_valuescol$", tolower(names(dots)))]
    codeBook_valueLabelsCol <- dots[grep("^codebook_valuelabelscol$", tolower(names(dots)))]
    recodeMissing <- dots[grep("^recode_missing$|^recodemissing$", tolower(names(dots)))]
    recodeMissingValue <- dots[grep("^recode_missingvalue$|^recodemissingvalue$", tolower(names(dots)))]
    convertString <- dots[grep("^convertstring$", tolower(names(dots)))]
} else if(length(dots_list) != 0 & !any(grepl("^codebook", tolower(names(dots_list))))){
	dots <- unlist(dots_list)
	variableValues <- dots[grep("^variablevalues", tolower(names(dots)))]
	valueLabels <- dots[grep("^valuelabels", tolower(names(dots)))]
	questionLabel <- dots[grep("^questionlabel$", tolower(names(dots)))]
	recodeMissing <- dots[grep("^recode_missing$|^recodemissing$", tolower(names(dots)))]
	recodeMissingValue <- dots[grep("^recode_missingvalue$|^recodemissingvalue$", tolower(names(dots)))]
	convertString <- dots[grep("^convertstring$", tolower(names(dots)))]
} else {
	stop(paste0("\nYou must either provide codebook inputs or supply user-defined inputs.\n\nCodebook inputs include:\ncodeBook\ncodeBook_variableCol\ncodeBook_questionLabelCol\ncodeBook_valuesCol\ncodeBook_valueLabelsCol\nrecodeMissing\n\n",
        "User-defined inputs include: \nvariableValues\nvalueLabels\nquestionLabel\nrecodeMissing"))
}

## subset data set by current column
dataSet <- dataSet[[variableName]]

## set these optional input to default
convertString <- if(!any(grepl("convertstring", tolower(names(dots))))){FALSE} else{as.logical(convertString)}
recodeMissing <- if(!any(grepl("^recode_missing$|^recodemissing$", tolower(names(dots))))){FALSE} else{as.logical(recodeMissing)}

# if recode missing is set to true
if(isTRUE(recodeMissing)){
	recodeMissingValue <- if(!any(grepl("^recode_missingvalue$|^recodemissingvalue$", tolower(names(dots))))){-9999}
else{recodeMissingValue}
}

## pull values/labels
# if using codebook
if(any(grepl("codebook", tolower(names(dots))))){
	# subset codebook by current variable
	codeBook <- eval(rlang::parse_expr(codeBook))
	codeBook <- codeBook[codeBook[codeBook_variableCol] == variableName,]
	codeBook <- codeBook[c(codeBook_variableCol, codeBook_questionLabelCol,codeBook_valuesCol,
	codeBook_valueLabelsCol)]

	# quick variable check
	if(nrow(codeBook) == 0) stop(paste0("Variable: ", variableName, " does not exist in the codebook you supplied."))

	# pull values
	values <- cerp::vulist(codeBook[[codeBook_valuesCol]])
	# pull value labels
	valuesNames <- cerp::vlulist(codeBook[[codeBook_valueLabelsCol]])
	# pull question label
	questionLabel <-  codeBook[[codeBook_questionLabelCol]]

	# Otherwise, extract supplied values/labels
} else {
  # pull values
  values <- unlist(variableValues)
  # pull value labels
  valuesNames <- unlist(valueLabels)
  # pull question label
  questionLabel <- questionLabel
 }

# final string check
# (if convertString is FALSE; treat all supplied values as double)
# note a default -9999 for missing is set
if(isFALSE(convertString)){
  dataSet <- as.double(dataSet)
  values <- as.double(values)
  recodeMissingValue <- ifelse(suppressWarnings(is.na(as.double(recodeMissingValue))),-9999, as.double(recodeMissingValue))
} else{
  # otherwise convert everything to character
  # added: convert blank ("") to NA
  dataSet[dataSet == ""] <- NA
  dataSet <- as.character(dataSet)
  values <- as.character(values)
  recodeMissingValue <- as.character(recodeMissingValue)
}

## add labels
if(recodeMissing){
  dataSet[is.na(dataSet)] <- recodeMissingValue
  haven::labelled_spss(x = dataSet, labels = stats::setNames(values, valuesNames), na_values = recodeMissingValue, label = questionLabel)
} else{
  haven::labelled_spss(x = dataSet, labels = stats::setNames(values, valuesNames), label = questionLabel)
}

}



