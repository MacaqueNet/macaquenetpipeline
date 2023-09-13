#' remove and replace accents in character vectors
#'
#' @param x character vector
#'
#' @return x with accents replaced
#' @export
#'
#' @examples
#' remove_accents(c("Émile", "André", "Bör", "Taï"))

remove_accents <- function(x) {
  x <- gsub(pattern = "\u00e9", replacement = "e", x = x)
  x <- gsub(pattern = "\u00c9", replacement = "E", x = x)
  x <- gsub(pattern = "\u00f6", replacement = "o", x = x)
  x <- gsub(pattern = "\u00e8", replacement = "e", x = x)
  x <- gsub(pattern = "\u00ef", replacement = "i", x = x)
  x <- gsub(pattern = "\u00ee", replacement = "i", x = x)
  x
}
