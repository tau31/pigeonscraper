# load(here::here("R", "sysdata.rda"))

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

