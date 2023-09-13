#' prepare datasets linked to a specific project
#'
#' @param project_code character, the id code for the needed project
#' @param output_folder character, the path where the zip file should be written.
#'                      The default is the working directory.
#' @param silent logical (default is \code{TRUE}), suppress the progress updates
#'               from read_subject_data and read_interaction_data.
#'               Set to \code{FALSE} for debugging.
#'
#' @return a zip is file is written
#' @export
#' @importFrom utils capture.output head packageVersion unzip write.table zip
#'
#' @examples
#' \dontrun{
#' export_project_data(project_code = NULL, output_folder = "~/Desktop")
#' }
#'

export_project_data <- function(project_code = NULL,
                                output_folder = NULL,
                                silent = TRUE) {

  # current package/database version
  pv <- as.character(packageVersion("macaquenetpipeline"))

  key <- get_key()

  if (is.null(output_folder)) output_folder <- normalizePath(getwd())

  # this is just a placeholder for now
  # eventually, we need extract the data sets that are required from some table ('project_table')

  if (is.null(project_code)) {
    data_sets <- key$running_number
    project_code <- "everything"
  }

  if (any(!data_sets %in% key$running_number)) stop("didn't find all data sets in the data key", call. = FALSE)

  key <- key[key$running_number %in% data_sets, ]

  # prep for output of 'meta' data
  out <- data.frame(project = project_code, generated_on = as.character(as.Date(Sys.Date())), package_version = as.character(pv), key)
  # remove not-needed columns
  out$package_file <- NULL
  out$datafile_exists <- NULL
  out$folder <- NULL
  out$file <- NULL
  out$sheet <- NULL
  out$sheetrange <- NULL
  out$sep <- NULL
  out$notes <- NULL
  out$has_obseff <- NULL
  out$group_name_verified <- NULL
  out$obseff_file <- NULL
  out$ready_to_deploy <- NULL
  out$crucial_issue_remaining <- NULL
  out$cat_detail <- NULL
  head(out)
  colnames(out)


  # create temp folder
  f <- file.path(tempdir(), "for_export/")
  dir.create(path = f, showWarnings = FALSE)
  # system2("open", f)

  # write meta data
  write.csv(x = out, file = file.path(f, "_metadata.csv"), quote = TRUE, row.names = FALSE)

  # copy subject and interaction data to R's temp folder
  for (i in 1:nrow(key)) {
    if (!file.exists(key$package_file[i])) {
      warning("didn't find", key$package_file[i], call. = FALSE)
    } else {
      file.copy(from = key$package_file[i], to = file.path(f, basename(key$package_file[i])))
    }
  }

  # zip all files
  # current wd:
  g <- getwd()
  # name for file:
  fn <- paste0(project_code, "_", pv, ".zip")
  fn <- file.path(output_folder, fn)
  setwd(f)
  zip(zipfile = fn, files = list.files(), flags = "-r9Xq")
  setwd(g)

  print(unzip(fn, list = TRUE))
  message("stored zipped data to ", shQuote(fn))
}
