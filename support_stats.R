library(httr)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(DT)

getPostByAPI <- function(site, post_id) {
    
    url_root <- switch(tolower(site), 
                       bioc = 'https://support.bioconductor.org',
                       biostars = 'https://www.biostars.org',
                       NULL)
    if(is.null(url_root)) { stop('Unknown site') }
    
    post_url <- paste0(url_root, '/api/post/', post_id)
    
    res <- content(httr::GET(url = post_url), type = 'application/json')
    
    return(as_tibble(res[ c('creation_date', 'id', 'root_id', 'title', 'url') ]))
}

getPostsByTag <- function(site = "bioc", tag = "biomaRt", n_pages = 10) {
    
    post_ids <- lapply(seq_len(n_pages), FUN = function(x) {
        
        url_root <- switch(tolower(site), 
                           bioc = 'https://support.bioconductor.org',
                           biostars = 'https://www.biostars.org',
                           NULL)
        
        if(is.null(url_root)) { stop('Unknown site') }
        
        url <- paste0(url_root, '/t/', tag, '/',
                      '?sort=Creation&limit=All%20time&answered=all&page=', x)
        
        page <- httr::GET(url)
        
        ## only carry on if we get a good page
        if(page$status_code != 200) {
            return(NULL)
        } else {
            
            tmp <- tempfile()
            writeLines(content(page, as = "text"), con = tmp)
            page <- readLines(con = tmp)
            
            post_lines <- page[grep(page, pattern = 'post-title')+2]
            
            if(!length(post_lines)) {
                return(NULL)
            } else {
                ids <- str_match(post_lines, '/p/([0-9]+)/')[,2]
                ids <- ids[ which(!is.na(ids)) ]
                ids
            }
        }
    })
    
    if(is.null(unlist(post_ids))) {
        return(NULL)
    } else {
    all_posts <- lapply(unlist(post_ids), getPostByAPI, site = site) %>%
        bind_rows() %>%
        mutate(creation_date = as.Date(creation_date), 
               site = site,
               tag = tag)
        return(all_posts)
    }
    
}

getPostsByUser <- function(site = 'bioc', user_id = '3986', n_pages = 20) {
    
    post_ids <- lapply(seq_len(n_pages), FUN = function(x) {
        
        url_root <- switch(tolower(site), 
                           bioc = 'https://support.bioconductor.org',
                           biostars = 'https://www.biostars.org',
                           NULL)
        
        url <- paste0(url_root, '/u/', user_id, '/',
                      '?sort=Creation&limit=All%20time&answered=all&page=', x)
        
        page <- httr::GET(url)
        
        ## only carry on if we get a good page
        if(page$status_code != 200) {
            return(NULL)
        } else {
            
            tmp <- tempfile()
            write_html(content(page), tmp)
            page <- readLines(tmp)
            
            post_lines <- page[grep(page, pattern = 'post-title')+4]
            
            if(!length(post_lines)) {
                return(NULL)
            } else {
                
                ids <- str_match(post_lines, '/p/[0-9]+/#([0-9]+)')[,2]
                ids <- ids[ which(!is.na(ids)) ]
                return(ids)
            }
        }
    })
    
    if(is.null(post_ids)) {
        return(NULL)
    } else {
    all_posts <- lapply(unlist(post_ids), getPostByAPI, site = site) %>%
        bind_rows() %>%
        mutate(creation_date = as.Date(creation_date), site = site)
    }
    
}

getGithubIssues <- function(user = 'grimbough', repo = "rhdf5") {
    
    url <- paste0('https://api.github.com/repos/', user, 
                  '/', repo, '/issues?state=all')
    
    json <- content(GET(url))
    creation_date <- vapply(json, function(x) return(x$created_at), character(1)) %>%
        as.Date()
    title <- vapply(json, function(x) return(x$title), character(1))
    url <- vapply(json, function(x) return(x$html_url), character(1))
    
    res <- tibble(creation_date = creation_date, 
                  tag = repo, 
                  site = "github", 
                  title = title,
                  url = url)
    return(res)
}


    

getBiocStats <- function(package = 'rhdf5') {
    url <- paste0('https://www.bioconductor.org/packages/stats/bioc/',
                 package, '/', package, '_stats.tab', sep = '')
#url <- paste0('~/',
#                  package, '_stats.tab.txt', sep = '')
    dl_stats <- read_delim(url, delim = '\t', col_types = c('ccii')) %>% 
        filter(Month != 'all') %>%
        mutate(Date = ymd(paste(Year, Month, '01', sep = '-')), Site = 'bioc', Package = package)
    dl_stats
}


    


    
    
