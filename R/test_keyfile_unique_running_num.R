#' @export

test_keyfile_unique_running_num <- function() {
  cat(silver("keyfile: there should be no duplicates in the running number\n"))
  res <- FALSE

  key <- get_key()
  sel <- which(duplicated(key$running_number))
  if (length(sel) == 0) {
    cat(green("all good\n"))
    res <- TRUE
  } else {
    cat(red("problems found:\n"))
    out <- key[key$running_number %in% key$running_number[sel], c("running_number", "unit_code", "species", "source", "group_name", "year")]
    print(out)
  }
  cat(silver("------------------------------------------\n"))
  invisible(res)
}
