#' build all data files
#'
#' read from raw data and write csv into inst/extdata/
#'
#' @param key optional key file (e.g. for selection of subset)
#' @param ready_to_deploy_only logical, only use data sets that are marked
#'        as ready for deployment in key file. Default is \code{FALSE}, i.e.,
#'        use all rows irrespective of their status.
#' @param silent logical, suppress info messages (default is \code{FALSE})
#' @param scramble logical (default is \code{TRUE}!!!): should the
#'        interaction data be scrambled (see
#'        \code{\link{read_interaction_data}} and
#'        \code{\link{scramble_matrix}})
#' @param use_seed should a seed be used for scrambling that depends on the
#'        sum of the matrix. Default is \code{TRUE}, i.e. the scrambled
#'        matrix will be the same each time the function is called. This
#'        only has an effect if \code{scramble = TRUE}.
#' @param reverse_order logical, reverse the order of entries in the key,
#'        i.e. read the most recent additions first, which makes debugging
#'        easier. Default is \code{FALSE}.
#'
#' @return nothing, but write files into inst/extdata/
#' @export
#'

build_all <- function(key = NULL,
                      ready_to_deploy_only = FALSE,
                      silent = FALSE,
                      scramble = TRUE,
                      use_seed = TRUE,
                      reverse_order = FALSE
                      ) {

  if (is.null(key)) {
    key <- get_key(read = TRUE, ready_to_deploy_only = ready_to_deploy_only)
  }

  if (reverse_order) {
    key <- key[rev(seq_len(nrow(key))), ]
  }

  # process subject data
  skey <- key[key$cat_global == "subjectdata", ]
  if (silent) {
    x <- capture.output(read_subject_data(key = skey))
  } else {
    read_subject_data(key = skey)
  }


  # process interaction data
  ikey <- key[key$cat_global != "subjectdata", ]
  if (silent) {
    x <- capture.output(read_interaction_data(scramble = scramble, key = ikey, use_seed = use_seed))
  } else {
    read_interaction_data(scramble = scramble, key = ikey, use_seed = use_seed)
  }


  if (scramble) {
    message("DATA WERE SCRAMBLED!!!")
  }

  message("don't forget to run 'match_subjects_interactions()'")
}
