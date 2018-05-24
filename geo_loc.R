library(DBI)
library(RSQLite)
library(dplyr)
library(iptools)
library(lubridate)


packages <- c("rhdf5", "Rhdf5lib", "beachmat", "biomaRt", "IONiseR", "BiocWorkflowTools", "DESeq", "DESeq2", "DEXSeq", "EBImage", "SIAMCAT")
query <- paste0('SELECT package,month_year,ips FROM access_log WHERE package IN ("',
                paste(packages, collapse = '","'),
                '")')

year_list <- list()
for(year in c('2015', '2016', '2017', '2018')) {
    
    message(year)
    
    ## open connection to database
    mydb <- dbConnect(RSQLite::SQLite(), paste0("/g/huber/users/msmith/download_db_", year, ".sqlite"))
    
    db_entries <- dbGetQuery(mydb, query) %>%
        as_tibble() 
    
    dbDisconnect(mydb)
    
    ## filter to only Germany
    ip_ranges <- iptools::country_ranges(countries = c("DE"))
    DE_idx <- parallel::mclapply(ip_ranges[["DE"]], FUN = function(ranges, ips) {
        
        which(ip_in_range(ranges = ranges, ip_addresses = ips))
        
    }, db_entries$ips, mc.cores = 20) %>% unlist()
    db_entries_DE <- db_entries[DE_idx,]
    
    ## list by package
    db_entries_list <- split(db_entries_DE, db_entries_DE$package)
    
    ## filter for unique IPs each month
    db_entries_unique <- lapply(db_entries_list, FUN = function(x) {
        x %>%
            group_by(month_year) %>%
         #   filter(grepl)
            filter(!duplicated(ips))
    })
    
    ## count number of IP and return summary table for all packages
    year_list[[ year ]] <- lapply(db_entries_unique, FUN = function(x) {
        x %>%
            summarize(Nb_of_distinct_IPs = n()) %>%
            mutate(Month = month(parse_date_time(month_year, "m/y"), label = TRUE, abbr = TRUE), 
                   Year = year(parse_date_time(month_year, "m/y"))) %>%
            arrange(Month) %>%
            select(Year, Month, Nb_of_distinct_IPs)
    }) %>%
        bind_rows(.id = "Package")
    
}

write.table(bind_rows(year_list), file = "/g/huber/users/msmith/denbi_reporting/DE_download_stats.tab", 
            sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

