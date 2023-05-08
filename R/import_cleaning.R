#' Loads a file given a format, with optional treatment of NA values
#'
#' @param filename The name of the file to be imported
#' @param filetype The type of the file ("csv" = (.csv), "excel" = (.xls, .xlsx) "text" = (txt))
#' @param removeNA a boolean to remove rows with NA values
#'
#' @return A dataframe
#'
#' @export
cleanImport <- function(filename, filetype, removeNA = TRUE) {

  cleanData()

  return(NULL)

}

#' Cleans the data in the data frame
#'
#' @param dataframe a dataframe to be cleaned
#' @param removeNA a boolean to remove rows with NA values
#'
#' @return A dataframe
cleanData <- function(filename, removeNA) {

  return(NULL)

}
