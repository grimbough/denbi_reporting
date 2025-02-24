getCondaStats <- function(packages = 'rhdf5', bioc = TRUE) {
  
  temp_file <- tempfile()
  url <- ifelse (bioc, 
                 'https://github.com/grimbough/anaconda-download-stats/raw/master/rdata/bioc_counts.rds',
                 'https://github.com/grimbough/anaconda-download-stats/raw/master/rdata/all_counts.rds'
  )  
  download.file(url, destfile = temp_file)
  
  tab <- readRDS(temp_file)
  if(bioc != TRUE) {
    tab <- tab %>% rename(Package = pkg_name, Year = year, 
                          Month = month, Nb_of_downloads = counts)
  }
  tab %>% 
    dplyr::filter(Package %in% packages) %>%
    mutate(Date = ymd(paste(Year, Month, '01', sep = '-')))
}

getCondaStats <- function(packages = 'rhdf5', bioc = TRUE) {
  readRDS(url('https://github.com/grimbough/anaconda-download-stats/raw/master/rdata/all_counts.rds')) %>%
    dplyr::filter(Package %in% packages) %>%
    mutate(Date = ymd(paste(Year, Month, '01', sep = '-')))
}