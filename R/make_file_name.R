#' prepare file names from key data
#'
#' @param unit_code character, the required unit code
#' @param cat_global the main data category (as per \code{key$cat_global})
#' @param cat_sub the first sub category (as per \code{key$cat_sub})
#' @param cat_detail the second sub category (as per \code{key$cat_detail})
#' @param data_type additional info (as per \code{key$data_type})
#' @param sampling_method additional info (as per \code{key$sampling_method})
#' @param folder character, default is \code{"inst/extdata/"}
#' @param file_ext character, default is \code{".csv"}
#'
#' @return a character (i.e. file and path name)
#'

make_file_name <- function(unit_code,
                           cat_global,
                           cat_sub = NULL,
                           cat_detail = NULL,
                           data_type = NULL,
                           sampling_method = NULL,
                           folder = "inst/extdata/",
                           file_ext = ".csv") {
  # template
  res <- paste0(unit_code, "_@_", cat_global)

  # subject data
  if ((is.null(cat_sub) & is.null(cat_detail)) | cat_global == "subjectdata") {
    return(paste0(folder, res, file_ext))
  }

  if (is.na(cat_detail)) cat_detail <- "NA"

  # interaction data
  res <- paste0(res, "_", cat_sub, "_", cat_detail, "_", data_type, "_", sampling_method)

  # if (is.na(cat_detail)) {
  #   res <- paste0(res, "_@_", cat_sub)
  # } else {
  #   res <- paste0(res, "_@_", cat_sub, "@@", cat_detail)
  # }

  paste0(folder, res, file_ext)
}
