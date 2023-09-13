#' remove data files from package directory
#'
#' @return a message
#' @importFrom utils askYesNo
#' @importFrom devtools has_tests
#' @export
#'
delete_data_files <- function() {
  keep <- c("datakey.csv", "datakey.xlsx", "_datakey.csv")
  cat("these files in", shQuote("inst/extdata"), "are (hopefully) kept if they exist:\n")
  cat(paste0(keep, collapse = "\n"), "\n")

  if (interactive()) {
    continue <- askYesNo(msg = "You really want to delete all data files?")
    if (continue) {
      ## xpath <- paste0(path.package("socialdiversitydata"), "/inst/extdata")
      xpath <- paste0("inst/extdata/")
      allfiles <- list.files(xpath)
      allfileswithpath <- list.files(xpath, full.names = TRUE)
      xkeep <- unlist(sapply(keep, function(X)which(allfiles == X)))
      toremove <- allfileswithpath[-c(xkeep)]
      formessage <- allfiles[-c(xkeep)]
      if (length(toremove) > 0) {
        file.remove(toremove)
        cat("files were removed:\n")
        cat(paste0(formessage, collapse = "\n"), "\n")
      }
    } else {
      cat("nothing done\n")
    }
  } else {
    cat("not in an interactive session\n")
  }
}
