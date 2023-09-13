#' read the key file to all data sets
#'
#' @param read logical, should the columns for processing the raw files be
#'        included (\code{TRUE} by default)
#' @param ready_to_deploy_only logical, only return data sets that are marked
#'        as ready for deployment in key file. Default is \code{FALSE}, i.e.,
#'        use all rows irrespective of their status.
#' @return a data.frame with the data sets (to be) included in the package
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' key <- get_key()
#' table(key$species)
#' colnames(key)
#'
#' # without the reading columns
#' key <- get_key(read = FALSE)
#' colnames(key)

get_key <- function(read = TRUE, ready_to_deploy_only = FALSE) {
  res <- read.csv(system.file("extdata/_datakey.csv",
                              package = "macaquenetpipeline"),
                  stringsAsFactors = FALSE)
  res <- as.data.frame(res)
  # retain only those with recorded category
  res <- res[res$cat_global != "", ]

  if (ready_to_deploy_only) {
    res <- res[res$ready_to_deploy == 1, ]
  }

  # create the data set code (matrix identifier)
  fn <- sapply(seq_len(nrow(res)), function(x) {
    make_file_name(unit_code = res$unit_code[x],
                   cat_global = res$cat_global[x],
                   cat_sub = res$cat_sub[x],
                   cat_detail = res$cat_detail[x],
                   data_type = res$data_type[x],
                   sampling_method = res$sampling_method[x]
    )
  })
  res$mat_id <- file_path_sans_ext(basename(fn))
  res$package_file <- file.path(system.file(package = "macaquenetpipeline"),
                                "extdata",
                                paste0(res$mat_id, ".csv"))
  res$datafile_exists <- file.exists(res$package_file)

  # file names for observation effort (applicable for interaction data only)
  res$obseff_file <- file.path(system.file(package = "macaquenetpipeline"),
                                "extdata",
                                paste0(res$mat_id, "_obseff.csv"))
  res$obseff_exists <- file.exists(res$obseff_file)
  res$obseff_file[res$cat_global == "subjectdata"] <- NA
  res$obseff_exists[res$cat_global == "subjectdata"] <- NA

  # columns relevant for reading raw files
  if (!read) {
    res$folder <- NULL
    res$file <- NULL
    res$sep <- NULL
    res$sheet <- NULL
    res$sheetrange <- NULL
  }
  res
}
