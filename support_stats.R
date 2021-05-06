library(httr)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(DT)

getPostFromID <- function(site, post_id) {
    
    url_root <- switch(tolower(site), 
                       bioc = 'https://support.bioconductor.org',
                       biostars = 'https://www.biostars.org',
                       NULL)
    if(is.null(url_root)) { stop('Unknown site') }
    
    post_url <- paste0(url_root, '/api/post/', post_id, '/')
    
    res <- content(httr::GET(url = post_url), type = 'application/json')
    
    res <- as_tibble(res[ c('creation_date', 'uid', 'title', 'url') ]) %>%
        mutate(creation_date = as_datetime(creation_date))

    return(res)
}

getPostIDsForTag <- function(site, tag) {
    
    url_root <- switch(tolower(site), 
                       bioc = 'https://support.bioconductor.org',
                       biostars = 'https://www.biostars.org',
                       NULL)
    if(is.null(url_root)) { stop('Unknown site') }
    
    post_url <- paste0(url_root, '/api/tag/', tag, '/')
    
    res <- content(httr::GET(url = post_url), type = 'application/json')
    
    ids <- unlist(res)
    return(ids)
}

getPosts <- function(site, tag, old_posts) {
    
    ids <- unlist(getPostIDsForTag(site = site, tag = tag))
    
    if(!is.null(old_posts)) {
        these_old_posts <- old_posts %>%
            filter(tag == !!tag, site == !!site)
        new_ids <- which(!ids %in% these_old_posts$uid)
    } else {
        these_old_posts <- NULL
        new_ids <- ids
    }
    
    res <- dplyr::bind_rows(lapply(new_ids, getPostFromID, site = site)) %>%
        mutate(tag = tag, site = site) %>%
        bind_rows(these_old_posts)
    res
    
}

getPostsByTag <- function(site = "bioc", tag = "biomaRt", n_pages = 10) {
    
    post_ids <- lapply(seq_len(n_pages), FUN = function(x) {
        
        url_root <- switch(tolower(site), 
                           bioc = 'https://support.bioconductor.org',
                           biostars = 'https://www.biostars.org',
                           NULL)
        
        if(is.null(url_root)) { stop('Unknown site') }
        
        url <- paste0(url_root, '/t/', tag, '/',
                      '?sort=creation&limit=all%20time&answered=all&page=', x)
        
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


getBiocPostsByTag <- function(tag = "biomaRt", pages = 1:2) {

    post_ids <- lapply(pages, FUN = function(x) {
        
        url_root <- 'https://support.bioconductor.org'
        
        url <- paste0(url_root,
                      '?type=latest&order=creation&tag=', tag, '&page=', x)
        
        page <- httr::GET(url)
        
        ## only carry on if we get a good page
        if(page$status_code != 200) {
            return(NULL)
        } else {
            
            tf <- tempfile()
            writeLines(content(page, as = "text"), con = tf)

            html <- xml2::read_html(tf)
            
            post_lines <- xml2::xml_find_all(html, 
                                      xpath = "//a[contains(@class, 'mini blue title header')]")

            if(!length(post_lines)) {
                return(NULL)
            } else {
                ids <- str_match(as.character(post_lines), '/p/(p?[0-9]+)/')[,2]
                ids <- ids[ which(!is.na(ids)) ]
                ids
            }
        }
    })
    
    if(is.null(unlist(post_ids))) {
        return(NULL)
    } else {
        all_posts <- lapply(unlist(post_ids), getPostByAPI, site = 'bioc') %>%
            bind_rows() %>%
            mutate(creation_date = as.Date(creation_date), 
                   site = 'bioc',
                   tag = tag)
        return(all_posts)
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
    dl_stats <- read_delim(url, delim = '\t', col_types = c('ccii')) %>% 
        filter(Month != 'all') %>%
        mutate(Date = ymd(paste(Year, Month, '01', sep = '-')), Site = 'bioc', Package = package)
    dl_stats
}

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


    
    
