# this file contains the individual functions that clean the input raw data
# (as supplied by the contributors)
#
# one technical note: these functions are not 'exported', i.e. they are not
# available in the usual way to the package end user
# during development the simplest solution is to call 'devtools::load_all()' to
# make the functions available

clean_delbruck <- function(xdata, id) {
  rn <- xdata[, 1]
  x <- as.matrix(xdata[, -1])
  rownames(x) <- rn
  non_focals <- c("e", "f", "g")
  x[non_focals, non_focals] <- NA
  diag(x) <- 0
  x
}

clean_hilbert <- function(xdata, id) {
  rn <- xdata[, 1]
  x <- as.matrix(xdata[, -1])
  colnames(x) <- remove_accents(colnames(x))
  rownames(x) <- remove_accents(rn)
  if (id == 1004) {
    x <- cbind(x[, c("a", "b")], c = 0, x[, c("d", "e", "f")])
  }
  if (any(rn != colnames(x))) stop("dim names mismatch in", id, call. = FALSE)
  diag(x) <- "0"
  x[is.na(x)] <- "0"
  x <- apply(x, 2, as.numeric)

  x
}

clean_noether <- function(xdata, id) {
  colnames(xdata) <- remove_accents(colnames(xdata))
  rownames(xdata) <- colnames(xdata)
  as.matrix(xdata)
}

