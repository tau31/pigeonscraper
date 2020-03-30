# Scrapping script -----

# tenho que passar
# group length
# output dir

group_length <- 3
n_workers <- 3


pigeon_scraper(3, 3)

future::plan(multiprocess(workers = 3))

pigeon_scraper(group_len = 3, n_workers = 3)
