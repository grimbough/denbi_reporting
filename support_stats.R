library(httr)
library(stringr)
library(dplyr)
library(lubridate)

getBiocPostByAPI <- function(post_id) {
    post_url <- paste0('https://support.bioconductor.org/api/post/', post_id)
    res <- content(httr::GET(url = post_url))
    return(as_tibble(res[ c('creation_date', 'id', 'root_id', 'tag_val', 'title') ]))
}

getBiostarsPostByAPI <- function(post_id) {
    post_url <- paste0('https://www.biostars.org/api/post/', post_id)
    res <- content(httr::GET(url = post_url))
    return(as_tibble(res[ c('creation_date', 'id', 'root_id', 'tag_val', 'title') ]))
}


getPostsByTag <- function(site = "bioc", tag = "biomaRt", pages = 10) {
    post_ids <- lapply(seq_len(pages), FUN = function(x) {
        
        url_root <- switch(tolower(site), 
                           bioc = 'https://support.bioconductor.org',
                           biostars = 'https://www.biostars.org',
                           )
        
        url <- paste0(url_root, '/t/', tag, '/',
                      '?sort=Creation&limit=All%20time&answered=all&page=', x)
        
        page <- readLines(url)
        
        post_lines <- page[grep(page, pattern = 'post-title')+2]
        
        ids <- str_match(post_lines, '/p/([0-9]+)/')[,2]
        ids <- ids[ which(!is.na(ids)) ]
        ids
    })
    
    all_posts <- lapply(unlist(post_ids), getBiocPostByAPI) %>%
        bind_rows() %>%
        mutate(creation_date = as.Date(creation_date))
    
}

getPostsByUser <- function(user_id = '3986') {
    
    post_ids <- lapply(1:20, FUN = function(x) {
    url <- paste0('https://support.bioconductor.org/u/', user_id, '/',
                  '?sort=Creation&limit=All%20time&answered=all&page=', x)
    
    page <- readLines(url)
    post_lines <- page[grep(page, pattern = 'post-title')+4]
    
    ids <- str_match(post_lines, '/p/[0-9]+/#([0-9]+)')[,2]
    ids <- ids[ which(!is.na(ids)) ]
    ids
    })
    
    all_posts <- lapply(unlist(post_ids), getBiocPostByAPI) %>%
        bind_rows() %>%
        mutate(creation_date = as.Date(creation_date))
    
}


posts.biomart <- getPostsByTag()
posts.rhdf5 <- getPostsByTag(tag = 'rhdf5', pages = 3)
posts.me <- getPostsByUser()
