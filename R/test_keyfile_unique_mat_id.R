#' @export

test_keyfile_unique_mat_id <- function() {
  cat(silver("keyfile: there should be no duplicates in the matrix ID\n"))
  res <- FALSE

  key <- get_key()
  sel <- which(duplicated(key$mat_id))
  if (length(sel) == 0) {
    cat(green("all good\n"))
    res <- TRUE
  } else {
    cat(red("problems found:\n"))
    out <- key[key$running_number %in% key$running_number[sel], c("running_number", "unit_code", "mat_id")]
    print(out)
  }
  cat(silver("------------------------------------------\n"))
  invisible(res)
}
