#' read raw interaction data sets
#'
#' @param set the key numbers (or by default read everything)
#' @param write logical, should the read data be written to the extdata folder?
#'        Default is \code{TRUE}. If \code{FALSE}, nothing is written and result
#'        of reading is returned.
#' @param key optional data frame with the key table
#' @param scramble logical (default is \code{FALSE}): should the interaction
#'        data be scrambled via \code{\link{scramble_matrix}}
#' @param use_seed should a seed be used for scrambling that depends on the sum of the matrix.
#'        Default is \code{TRUE}, i.e. the scrambled matrix will be the same
#'        each time the function is called. This only has an effect if
#'        \code{scramble = TRUE}.
#' @param output_folder character, the path where the csv files should be written.
#'                      The default is inside the package directory
#'                      (\code{"inst/extdata/"}).
#'
#' @importFrom utils read.table write.csv read.csv
#' @importFrom readxl read_excel
#' @importFrom gtools mixedorder
#'
#' @return writes files and returns a message. Or returns an interaction matrix.
#' @export
#'

read_interaction_data <- function(set = NULL,
                                  write = TRUE,
                                  key = NULL,
                                  scramble = FALSE,
                                  use_seed = FALSE,
                                  output_folder = "inst/extdata/") {
  # read the key file
  if (is.null(key)) {
    key <- get_key(read = TRUE)
  }

  # this is the list of behavior categories we currently support
  # we might eventually merge (split?) some of them
  interactiondata <- c("bodycon",
                       "conagg",
                       "groom",
                       "nonconagg",
                       "prox",
                       "aggr",
                       "subm",
                       "suppl",
                       "displace")

  # filter (i.e. remove subject-level entries)
  key <- key[key$cat_global %in% c("affi", "aggr"), ]

  # report if set (if selected is not found)
  if (any(!set %in% key$running_number)) {
    stop("didn't find the specified set(s)")
  }

  # if set is left blank, run all matrices
  if (is.null(set)) set <- key$running_number

  i = set[1]
  for (i in set) {
    cat("\n")
    keyline <- which(key$running_number == i)
    cat("processing key line:", keyline, "; set:", i, "; unit_code:", shQuote(key$unit_code[keyline]), "\n")
    cat("-------\n")
    cat("cats: ", key$cat_global[keyline], ", ", key$cat_sub[keyline], ", ", key$cat_detail[keyline], "\n")
    cat("sampling method: ", key$sampling_method[keyline], "\n")
    cat("data type: ", key$data_type[keyline], "\n")
    cat("corrected for observation effort: ", key$corrected_obseff[keyline], "\n")
    cat("required transposing: ", key$required_transposing[keyline], "(1 means matrix needed transposing during import (at the end of read_interaction_data function) and is shown correctly here) \n")
    # separator
    xsep <- key$sep[keyline]
    # read raw file
    fn <- paste0("data-raw/original_data_files/",
                 key$folder[keyline],
                 "/",
                 key$file[keyline])
    somethingwasread <- FALSE # keep track of potentially weird file extensions

    if (xsep == "comma") {
      xdata <- read.table(file = fn,
                          sep = ",",
                          header = TRUE,
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
      cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (xsep == "semicolon") {
      xdata <- read.table(file = fn,
                          sep = ";",
                          header = TRUE,
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
      cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (xsep == "tab") {
      xdata <- read.table(file = fn,
                          sep = "\t",
                          header = TRUE,
                          check.names = FALSE)
      cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (xsep == "excel") {
      # sheet range
      if (!is.na(key$sheetrange[keyline])) {
        R <- key$sheetrange[keyline]
      } else {
        R <- NULL
      }
      # messages (not warnings/errors) that are suppressed relate to
      # funky column names
      suppressMessages(
        xdata <- data.frame(read_excel(path = fn,
                                       sheet = key$sheet[keyline],
                                       range = R),
                            stringsAsFactors = FALSE)
      )
      cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (xsep == "manual") {

    }


    # report if at this point no file was actually read
    if (!somethingwasread) {
      cat(key$file[keyline], "not read... \n")
    }

    # cleaning section
    # this run the provider-specific cleaning files
    somethingwascleaned <- FALSE

    if (key$source[keyline] == "delbruck") {
      xdata <- clean_delbruck(xdata, id = i)
      somethingwascleaned <- TRUE
    }

    if (key$source[keyline] == "hilbert") {
      xdata <- clean_hilbert(xdata, id = i)
      somethingwascleaned <- TRUE
    }

    if (key$source[keyline] == "noether") {
      xdata <- clean_noether(xdata, id = i)
      somethingwascleaned <- TRUE
    }



    if (!somethingwascleaned) {
      cat("nothing cleaned in key line:", keyline, "; set:", i, "\n")
    }

    # transpose data if indicated in key file
    # so that givers/actors are in the rows (and receivers are in columns)
    # which is what Delphine and Christof would consider their personal default...
    if (!is.na(key$required_transposing[keyline])) {
      if (key$required_transposing[keyline] == 1) {
        xdata <- t(xdata)
      }
    }


    # run some superficial checks
    somechecks <- check_names(xdata)

    # scramble data if desired
    if (scramble) {
      xdata <- scramble_matrix(mat = xdata, use_seed = use_seed)
    }

    # make col and row names lower case
    colnames(xdata) <- tolower(colnames(xdata))
    rownames(xdata) <- tolower(rownames(xdata))

    # save files (unless there were issues with check_names above)
    # once as RData and once as csv
    # Rdata: I'll skip this for now...
    if (FALSE %in% somechecks) {
      warning("there were problems with ", shQuote(key$file[keyline]), "\n")
    } else {

      # fn <- paste0(key$unit_code[keyline],
      #              "_@_", key$cat_global[keyline])
      if (write) {
        # set the file name
        fn <- make_file_name(unit_code = key$unit_code[keyline],
                             cat_global = key$cat_global[keyline],
                             cat_sub = key$cat_sub[keyline],
                             cat_detail = key$cat_detail[keyline],
                             data_type = key$data_type[keyline],
                             sampling_method = key$sampling_method[keyline],
                             folder = output_folder)
        # write csv
        write.csv(x = xdata, file = fn, row.names = FALSE, quote = FALSE)
        cat("written successfully: ", shQuote(basename(fn)), "\n--------------------\n", sep = "")
      } else {
        # if option write=FALSE just return the cleaned matrix
        return(xdata)
      }
    }
  }
}
