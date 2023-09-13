#' match ids in interaction data to subject-level data
#'
#' @param unit_code character, the required unit code (if not supplied, all
#'        units found in \code{key} are used)
#' @param key the data key (if not supplied it's read from the package data)
#'
#' @return (re)writing interaction matrices and also an invisible matrix with
#'         reports as to who was removed and added from which table/matrix
#' @importFrom crayon red silver bold underline cyan blue magenta
#' @export
#'

match_subjects_interactions <- function(unit_code = NULL,
                                        key = NULL) {
  if (is.null(key)) {
    key <- get_key(read = FALSE)
  }

  if (is.null(unit_code)) {
    unit_code <- unique(key$unit_code)
  }

  out <- matrix(ncol = 4, nrow = 0, "")

  u="nigra_noether_g1_1996_1"
  for (i in seq_len(length(unit_code))) {
    u <- unit_code[i]
    # read subject data
    xfile <- key$package_file[key$unit_code == u & key$cat_global == "subjectdata"]
    if (file.exists(xfile)) {
      sdata <- read.csv(xfile)
    } else {
      cat("didn't process", shQuote(u), "\n")
      sdata <- NULL
    }
    if (!is.null(sdata)) {
      cat("\n", underline(bold(u)), "\n")

      # get the actual subjects
      # subadults and adults from subjectdata
      # juveniles and infants are already excluded when the subject data are read
      all_subs <- sdata$subject
      # now interactants
      # interaction matrices
      xfiles <- key$package_file[key$unit_code == u &
                                   key$cat_global != "subjectdata"]
      interacting_ids <- lapply(xfiles, function(x) {
        unique(colnames(read.csv(x, check.names = FALSE)))
        })
      interacting_ids <- table(as.character(unlist(interacting_ids)))
      # and get the final ids
      # and since we don't remove any lines from the subject data (anymore)
      #.   final_ids are just all the subjects
      # final_ids <- sort(intersect(all_subs, names(interacting_ids)))
      final_ids <- sort(sdata$subject)
      final_ids_for_report <- sort(intersect(all_subs, names(interacting_ids)))

      # deal with subject data
      to_report <- sdata$subject[!sdata$subject %in% final_ids_for_report]
      fn <- key$package_file[key$unit_code == u &
                               key$cat_global == "subjectdata"]
      if (length(to_report) > 0) {
        out <- rbind(out, cbind(u, basename(fn), "removed", to_report))
        cat(bold(red("kept IDs in the subject data that never ever actually interacted with anyone:")), red(paste0(sort(to_report), collapse = ", ")), "\n")
        warning("should we remove IDs from subject data: ", paste0(to_report, collapse = ", "), " ? in ", shQuote(u), "\nplease check!!!")
        # sdata <- sdata[sdata$subject %in% final_ids, ]
        # write.csv(sdata, file = fn, row.names = FALSE, quote = FALSE)
      } else {
        cat(silver("nothing removed from subject data\n"))
      }

      # deal with interaction data
      k = xfiles[3]
      for (k in xfiles) {
        # cat("----\n")
        # cat("rewrote interaction data:", basename(k), "\n")

        temp <- read.csv(k, check.names = FALSE)
        # which subjects are not present in interaction data, if any
        # although they should
        missing_ids <- final_ids[!final_ids %in% colnames(temp)]
        # add columns for them
        if (length(missing_ids) > 0) {
          out <- rbind(out, cbind(u, basename(k), "added", missing_ids))
          cat(red(basename(k), ":"), bold(red("added IDs to interaction data:")), red(paste0(missing_ids, collapse = ", ")), "\n")
          # stop()
          for (j in missing_ids) {
            temp <- cbind(temp, 0)
            temp <- rbind(temp, 0)
            colnames(temp)[ncol(temp)] <- j
          }
        } else {
          cat(silver(basename(k), ":", "nothing added\n"))
        }
        # check for removed ids from interaction data
        removed_ids <- colnames(temp)[!colnames(temp) %in% final_ids]
        if (length(removed_ids) > 0) {
          out <- rbind(out, cbind(u, basename(k), "removed", removed_ids))
          cat(red(basename(k), ":"), red(bold("removed IDs from interaction data:")), red(paste0(removed_ids, collapse = ", ")), "\n")
        } else {
          cat(silver(basename(k), ":", "nothing removed\n"))
        }

        # reorder
        rownames(temp) <- colnames(temp)
        temp <- temp[final_ids, final_ids]
        # if any partner_only ids are there: set interactions among them to NA
        if (any(sdata$partner_only == 1)) {
          # get the partners
          partner_ids <- sdata$subject[which(sdata$partner_only == 1)]
          if (length(partner_ids) > 1) {
            # j = partner_ids[2]
            for (j in partner_ids) {
              temp[j, partner_ids] <- NA
              temp[partner_ids, j] <- NA
            }
            diag(temp) <- 0
          }
        }

        # rewrite csv
        write.csv(temp, file = k, row.names = FALSE, quote = FALSE)

      }
      cat("------------------------------------------------------------------\n\n")
    }
  }

  invisible(out)
}
