#' test internal consistency of the key file
#'
#' @importFrom crayon green
#' @return textual messages (and invisible logical vector)
#' @export
#'

test_keyfile <- function() {
  test_keyfile_unit_codes()
  test_keyfile_unique_running_num()
  test_keyfile_unique_mat_id()
}
