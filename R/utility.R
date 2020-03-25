# Create index for css queries ---------------------------

#' Create index to iterate over css queries by groups of n queries.
#'
#' Takes in a data.frame of css queries to be submitted using RSelenium and
#' outputs a matrix with start and end index used to further loop over.
#'
#' @param seq_length integer specifying the size of the groups
#' @param rows integer with number of queries to be submitted.
#'
#' @return matrix with two columns with start and end end indexes.

create_start_end <- function(seq_length, rows) {
  seq_length <- 3
  start <- seq(1,rows, by = seq_length)
  n_groups <- length(start)
  end <- c(start[1:(n_groups - 1)] + (seq_length-1),
           (start[n_groups] - 1) + (rows - (start[n_groups] - 1)))
  return(cbind(start, end))
}


