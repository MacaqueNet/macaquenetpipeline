#' @export

test_keyfile_unit_codes <- function() {
  cat(silver("keyfile: unit codes should match the rule species_source_groupname_year_period\n"))
  res <- FALSE

  key <- get_key()
  unit_code_by_hand <- paste(key$species, tolower(key$source), tolower(key$group_name), key$year, key$within_year_period, sep = "_")
  if (all(unit_code_by_hand == key$unit_code)) {
    cat(green("all good\n"))
    res <- TRUE
  } else {
    cat(red("problems found:\n"))
    sel <- which(key$unit_code != unit_code_by_hand)
    key$should_be <- unit_code_by_hand
    key$current_unit_code <- key$unit_code
    out <- key[sel, c("should_be", "current_unit_code", "species", "source", "group_name", "year", "within_year_period")]
    print(out)
  }
  cat(silver("------------------------------------------\n"))
  invisible(res)
}

