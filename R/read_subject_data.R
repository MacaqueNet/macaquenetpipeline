#' read subject data (observation time, age, sex etc)
#'
#' @param set the key numbers (or by default read every subject data found)
#' @param write logical, should the read data be written to the \code{extdata}
#'        folder? Default is \code{TRUE}. If \code{FALSE}, nothing is
#'        written and result of reading is returned as data.frame.
#' @param key optional data frame with the key table
#' @param silent logical, suppress info messages (default is \code{FALSE})
#' @param output_folder character, the path where the csv files should be written.
#'                      The default is inside the package directory
#'                      (\code{"inst/extdata/"}).
#'
#' @return writes files and returns a message (or a data.frame)
#' @export
#'
#' @importFrom stats na.omit

read_subject_data <- function(set = NULL,
                              write = TRUE,
                              key = NULL,
                              silent = FALSE,
                              output_folder = "inst/extdata/") {
  if (is.null(key)) {
    key <- get_key(read = TRUE)
  }

  subjectdata <- c("subjectdata")
  key <- key[key$cat_global %in% subjectdata, ]

  if (is.null(set)) set <- key$running_number

  i = set[1]
  for(i in set) {
    #######################
    # read file -> generic part for all data sets (except when extracted manually)
    #######################

    if (!silent) cat("\n")
    keyline <- which(key$running_number == i)
    xsep <- key$sep[keyline]
    # read raw file
    fn <- paste0("data-raw/original_data_files/",
                 key$folder[keyline],
                 "/",
                 key$file[keyline])
    somethingwasread <- FALSE

    if (xsep == "comma") {
      xdata <- read.table(file = fn, sep = ",",
                          header = TRUE, check.names = FALSE)
      if (!silent) cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (xsep == "semicolon") {
      xdata <- read.table(file = fn, sep = ";",
                          header = TRUE, check.names = FALSE)
      if (!silent) cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (xsep == "tab") {
      xdata <- read.table(file = fn, sep = "\t",
                          header = TRUE, check.names = FALSE)
      if (!silent) cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (xsep == "excel") {
      if (!is.na(key$sheetrange[keyline])) {
        R <- key$sheetrange[keyline]
      } else {
        R <- NULL
      }

      xdata <- data.frame(read_excel(path = fn,
                                     sheet = key$sheet[keyline],
                                     range = R,
                                     .name_repair = "minimal"))
      if (!silent) cat("read sucessfully:", key$file[keyline], "\n")
      somethingwasread <- TRUE
    }

    if (!somethingwasread & !is.na(key$file[keyline])) {
      if (!silent) cat(key$file[keyline], "not read... \n")
    }

    if (is.na(key$file[keyline]) & key$sep[keyline] == "manual") {
      if (!silent) cat("produced subject data manually\n")
    }

    #######################
    # deal with the data set specifics
    #######################
    if (key$source[keyline] == "noether") {
      xdata <- data.frame(unit_code = key$unit_code[keyline],
                      subject = remove_accents(xdata$id),
                      sex = tolower(xdata$sex),
                      age = NA,
                      age_cat = xdata$age,
                      obseff_duration = xdata$focalhours,
                      partner_only = 0,
                      is_adult = 1
                      )
      xdata$is_adult[xdata$age %in% c("juvenile")] <- 0
    }

    if (key$source[keyline] == "hilbert") {
      xdata <- data.frame(unit_code = key$unit_code[keyline],
                          subject = remove_accents(xdata$id),
                          sex = tolower(xdata$sex),
                          age = NA,
                          age_cat = xdata$age,
                          obseff_duration = xdata$focalhours,
                          partner_only = 0,
                      is_adult = 1
      )
    }

    if (key$source[keyline] == "delbruck") {
      xdata <- data.frame(unit_code = key$unit_code[keyline],
                          subject = xdata$Monkey.name,
                          sex = substr(tolower(xdata$sex), 1, 1),
                          age = NA,
                          age_cat = NA,
                          obseff_duration = xdata$Observation.time,
                          partner_only = as.numeric(xdata$Observation.time == 0),
                          is_adult = 1
      )
    }





    # remove immatures from subject data
    xdata <- xdata[which(!xdata$age_cat %in% c("infant", "juvenile")), ]

    # add optional columns for obseff
    if (!"obseff_duration" %in% colnames(xdata)) {
      xdata$obseff_duration <- NA
    }
    if (!"obseff_duration_2" %in% colnames(xdata)) {
      xdata$obseff_duration_2 <- NA
    }
    if (!"obseff_samples" %in% colnames(xdata)) {
      xdata$obseff_samples <- NA
    }
    if (!"obseff_samples_2" %in% colnames(xdata)) {
      xdata$obseff_samples_2 <- NA
    }

    # add required columns partner_only and is_adult
    if (!"partner_only" %in% colnames(xdata)) {
      xdata$partner_only <- NA
    }
    if (!"is_adult" %in% colnames(xdata)) {
      xdata$is_adult <- NA
    }

    # make textual columns to lower-case
    xdata$subject <- tolower(xdata$subject)
    xdata$sex <- tolower(xdata$sex)
    xdata$age_cat <- tolower(xdata$age_cat)

    # make sure column order is standardized...
    xdata <- xdata[, c("unit_code", "subject", "sex", "age", "age_cat", "is_adult", "partner_only",
                       "obseff_samples", "obseff_samples_2", "obseff_duration", "obseff_duration_2")]

    # reset row names
    rownames(xdata) <- NULL

    #######################
    # 'export' data
    #######################
    # save files (unless there were issues above)
    # once as RData and once as csv
    # Rdata: I'll skip this for now...
    # fn <- paste0(key$unit_code[keyline], "_", key$cat_global[keyline])
    if (write) {
      # create file name
      fn <- make_file_name(unit_code = key$unit_code[keyline],
                           cat_global = key$cat_global[keyline],
                           folder = output_folder)
      # write csv file
      write.csv(x = xdata, file = fn, row.names = FALSE, quote = FALSE)
      if (!silent) cat("written successfully: ", shQuote(basename(fn)), "\n--------------------\n", sep = "")
    } else {
      return(xdata)
    }
  }
}
