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

# future plan multiprocess with user defined number of processes.

#' Function to plan parallelization plan.
#'
#' In order to parallize multiple scraping tasks over many different cores, we
#' need to set our parallization plan. By default, \code{pigeonscraper} uses
#' \code{plan(multiprocess)} and the user can set the number of cores to be used
#' by setting the parameter workers.
#'
#' @param workers number of cores to be used in parallel.
#'
#' @importFrom future multiprocess

plan_future <- function(workers){
  options(mc.cores = workers)
  cat(paste0("Using ", future::availableCores("mc.cores"), " cores \n"))
  future::plan(multiprocess)
}
