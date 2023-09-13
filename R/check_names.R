#' check column names and dimensions of data sets
#'
#' @param xdata data set
#'
#' @return a vector of length 3 where the first item indicates that column names are ok (no duplicates)
#'  the second checks whether column and row names (if there are any) match
#'  the third indicates that it's a square matrix
#' @export
#'

check_names <- function(xdata) {
  passed_checks <- c(noduplcols = FALSE, colrowmatch = FALSE, squaremat = FALSE)

  xtab <- table(colnames(xdata))
  if (max(xtab) > 1) {
    cat("duplicated column names:", names(xtab)[xtab > 1], "\n")
  } else {
    passed_checks["noduplcols"] <- TRUE
  }

  if (!is.null(dimnames(xdata)[[1]])) {
    if (!all(colnames(xdata) == rownames(xdata))) {
      cat("column and row names do not match\n")
    } else {
      passed_checks["colrowmatch"] <- TRUE
    }
  } else {
    passed_checks["colrowmatch"] <- NA
  }

  if (ncol(xdata) == nrow(xdata)) {
    passed_checks["squaremat"] <- TRUE
  }

  passed_checks
}
