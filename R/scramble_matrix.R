#' data scrambling for pseudo-blinding
#'
#'
#' @param mat square matrix to be scrambled
#' @param use_seed should a seed be used that depends on the sum of the matrix.
#'        Default is \code{TRUE}, i.e. the scrambled matrix will be the same
#'        each time the function is called.
#'
#' @return scrambled input matrix
#' @export
#'
#' @details The scrambling works by first selecting a random dyad for which at
#'          least one its two values is not zero. It then selects one more dyad,
#'          which may or may not be a zero-dyad. It then assigns the two
#'          values of the first dyad to the cells of the second dyad,
#'          and vice versa.
#'          As such, the total sum of a matrix is kept constant, but
#'          row and column sums will change from the input matrix.
#'
#' @importFrom stats runif

scramble_matrix <- function(mat, use_seed = TRUE) {
  n <- seq_len(ncol(mat))
  # indices of non-zero cells
  imat <- which(mat > 0, arr.ind = TRUE)

  if (nrow(imat) >= 1) {
    # indices of upper triangle
    umat <- which(upper.tri(mat), arr.ind = TRUE)

    if (use_seed) {
      set.seed(round(sum(as.numeric(mat), na.rm = TRUE)))
    }

    for (i in n) {
      # indices <- sample(n, 2)
      # select dyad and store values
      s_indices <- as.numeric(imat[sample(nrow(imat), 1), ])
      s_vals <- c(mat[s_indices[1], s_indices[2]], mat[s_indices[2], s_indices[1]])
      # select target dyad (which may or may not be a zero dyad) and store values
      t_indices <- umat[sample(nrow(umat), 1), ]
      t_vals <- c(mat[t_indices[1], t_indices[2]], mat[t_indices[2], t_indices[1]])

      # swap source values once in a while
      if (runif(1) > 0.7) s_vals <- rev(s_vals)

      # replace values in matrix
      mat[t_indices[1], t_indices[2]] <- s_vals[1]
      mat[t_indices[2], t_indices[1]] <- s_vals[2]
      mat[s_indices[1], s_indices[2]] <- t_vals[1]
      mat[s_indices[2], s_indices[1]] <- t_vals[2]
    }

  }
  set.seed(NULL)
  mat
}
