# Scrapping script -----

start_chrome_remDr(kill = TRUE)

remDr <- connect_remDr()

css_query_tbl <- pigeon_query_builder(remDr)

pigeon_scrapper(css_query_tbl[1,], remDr)

