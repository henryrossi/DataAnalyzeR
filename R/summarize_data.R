#' Return a summary of two variables of a given data frame
#'
#' @param dataFrame The name of the data frame
#' @param var1 A quoted variable name from the data frame
#' @param var2 A second quoted variable name from the data frame
#'
#' @return A vector of tables containing the summaries of the variables
#'
#' @export

summarizeData <- function(dataFrame, var1, var2) {
  if (class(dataFrame) != "data.frame") {
    stop("Must provide a data frame")
  }
  if (!(var1 %in% colnames(dataFrame) && var2 %in% colnames(dataFrame))) {
    stop("Must provide valid column names from the data frame.
         Make sure you provide the column names inside quotes")
  }
  returnVec <- c()
  for (var in c(var1, var2)) {
    if (class(dataFrame[[var]]) == "raw"){
      stop("This function is not compatible with data of the type 'raw'")
    }
    else if (class(dataFrame[[var]]) %in% c("numeric", "integer", "complex")) {
      returnVec <- append(returnVec, summary(dataFrame[[var]]))
    }
    else if (class(dataFrame[[var]]) %in% c("character", "logical")) {
      returnVec <- append(returnVec, prop.table(table(dataFrame[[var]])))
    }
  }
  return(returnVec)
}
