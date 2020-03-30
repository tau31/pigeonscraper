# Scrapping script -----

start_chrome_remDr(kill = TRUE)

remDr <- connect_remDr()

css_query_tbl <- pigeon_query_builder(remDr)

start_end <- create_start_end(3, nrow(css_query_tbl))

start <- start_end[,1]
end <- start_end[,2]

tail(start_end)
purrr::map(
  1,
  function(i){
    purrr::map(start[i] : end[i],
        purrr::safely(function(g) {
          pigeon_scrapper(css_query_tbl[g, ], remDr)
        }))
  }
)

