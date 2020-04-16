context("Unit testing the behavior of transforming state names into
        abbreviations in strings, transforming arrival times into
        periods and computing the difference between arrival and departure
        times")

test_that("Transforming state names to state abbreviations works in one city
          names ", {
            test <- "11/02/2019  --  Greenville Texas -- 08:15"
            result <- state_abb_trans(test)
            expect_equal(result, "11/02/2019  --  GREENVILLE TX -- 08:15")
          })

test_that("Transforming state names to state abbreviations works in two city
          names ", {
            test <- "01/18/2020  --  LAKE CITY, FLORIDA -- 07:50"
            result <- state_abb_trans(test)
            expect_equal(result, "01/18/2020  --  LAKE CITY, FL -- 07:50")
          })

test_that(
  "Function only transforms second occurrence of the state name in case city has
  a state name",{
    test <- "09/21/2019  --  IDAHO FALLS, IDAHO -- 10:50"
    result <- state_abb_trans(test)
    cat("function output: \n")
    cat(result, "\n")
    expect_equal(result, "09/21/2019  --  IDAHO FALLS, ID -- 10:50")
  }
)

test_that(
  "Function replaces state name correctly when state appears on second word",{
    test <- "08/27/2016  --  FERNLEY NEVADA -- 07:30"
    result <- state_abb_trans(test)
    cat("function output: \n")
    cat(result, "\n")
    expect_equal(result, "08/27/2016  --  FERNLEY NV -- 07:30")
  }
)

# arrival_tf --------------------------------------------------------------
test_data <- tibble(
  arrival = c("-11:16:15.6", "-11:16:15.6..", "11:16:15.6..", "11:16:15:6")
)

test_that(
  "when a string has multiple characthers before and after time, arrival_tf
  handles them correctly", {
    tf_test <- test_data %>%
      mutate(arrival = arrival_tf(arrival)) %>%
      distinct()
    expect_equal(nrow(tf_test), 1)
    expect_equal(tf_test$arrival, lubridate::hms("11:16:15.6"))
  })

test_data <- tibble(
  arrival = c("11:16:15:6","+11:16:15:6", "11:16:15:6..", "+11:16:15:6..")
)

test_that(
  "when a string has multiple characthers, before and/or after time, together
  with the wrong characther for seconds decimals, arrival_tf handles these
  cases correctly", {
    tf_test <- test_data %>%
      mutate(arrival = arrival_tf(arrival)) %>%
      distinct()
    expect_equal(nrow(tf_test), 1)
    expect_equal(tf_test$arrival, lubridate::hms("11:16:15.6"))
  })


# test race_time function -------------------------------------------------

test_that("difference between departure and arrival time is correctly computed
          by diff_time fun", {
            test_data <-
              tibble(
                departure = lubridate::hms(c("08:00:00", "08:00:00")),
                arrival = lubridate::hms(c("13:40:00", "07:42:00"))
              )
            data_tf <- test_data %>%
              mutate(race_time = diff_time(arrival = arrival,
                                           departure = departure))
            expect_equal(data_tf$race_time[1], 340)
            expect_equivalent(data_tf$race_time[2], 462)

          })
