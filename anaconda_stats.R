library(dplyr)
library(lubridate)
library(arrow)
library(stringr)

input <- "20180501"

month_start <- ymd(input)

n_days <- lubridate::days_in_month(month_start)

month <- month_start %>%
  month() %>%
  stringr::str_pad(2, pad = "0")

year <- year(month_start) %>%
  as.character()

res <- vector("list", length = n_days)

for(day in seq_len(n_days)) {
  
  message(day)
  
  url <- paste0("https://anaconda-package-data.s3.amazonaws.com/conda/hourly/", 
                year, "/", month, "/", 
                year, "-", month, "-", stringr::str_pad(day, 2, pad = "0"), ".parquet")
  
  tf <- tempfile()
  
  download.file(url = url, destfile = tf)
  
  res[[day]] <- read_parquet(tf) %>% 
    filter(grepl(pkg_name, pattern = "^bioconductor")) %>% 
    group_by(pkg_name) %>% 
    summarise(total_count = sum(counts)) 
  
}

final <- bind_rows(res) %>%
  group_by(pkg_name) %>%
  summarise(count = sum(total_count))

saveRDS(final, file = paste0( substr(month_start, 1,7), ".rds" ))
