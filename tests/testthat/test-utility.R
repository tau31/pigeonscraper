skip_on_cran()

context("Evaluating function to create index for parallel scrapping")


test_that("max indexing number is equal to the number of rows", {
  seq_length = 3
  rows <- 20
  ind <- create_start_end(seq_length, rows)
  expect_equal(max(ind), 20)
})

test_that("sum o number of group elements equals to number of queries", {
  seq_length = 3
  rows <- 20
  ind <- create_start_end(seq_length, rows)
  n_sum <- ind[1,1]
  for (i in 2:nrow(ind)) {
    n_sum <- n_sum + (ind[i,1] - ind[i - 1,1] )
  }
  n_sum <- n_sum + (max(ind) - n_sum)
  expect_equal(unname(n_sum), rows)
})
