library(httr)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)

getPostByAPI <- function(site, post_id) {
    
    url_root <- switch(tolower(site), 
                       bioc = 'https://support.bioconductor.org',
                       biostars = 'https://www.biostars.org',
                       NULL)
    if(is.null(url_root)) { stop('Unknown site') }
    
    post_url <- paste0(url_root, '/api/post/', post_id)
    res <- content(httr::GET(url = post_url))
    return(as_tibble(res[ c('creation_date', 'id', 'root_id', 'title') ]))
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


getGithubIssues <- function(repo = "rhdf5") {
    tmp <- content(GET('https://api.github.com/repos/grimbough/rhdf5/issues?state=all'))
    tmp2 <- lapply(tmp, function(x) return(tibble(creation_date = as.Date(x$created_at))))
    
    res <- tibble(creation_date = pull(bind_rows(tmp2), creation_date), tag = repo, site = "github")
    return(res)
}

posts.biomart <- bind_rows(getPostsByTag(tag = 'biomaRt', site = "bioc"), 
                           getPostsByTag(tag = 'biomaRt', site = 'biostars'),
                           getPostsByTag(tag = 'rhdf5', site = "bioc", n_pages = 4), 
                           getPostsByTag(tag = 'rhdf5', site = 'biostars', n_pages = 1),
                           getPostsByTag(tag = 'IONiseR', site = "bioc", n_pages = 3), 
                           getPostsByTag(tag = 'IONiseR', site = 'biostars', n_pages = 1))

posts.biomart %>% 
    bind_rows(posts.biomart, tmp3) %>%
    filter(year(creation_date) >= 2015) %>% 
    ggplot(aes(fill = site, x = year(creation_date))) + 
    geom_bar() +
    scale_fill_manual(values = c('#1a81c2', '#8f2c47', 'grey'),
                      name = "Site",
                      labels = c("bioconductor.org", "biostars.org", "github.com")) + 
    theme_bw() + 
    xlab('Year') +
    facet_wrap(~ tag) 
    


posts.rhdf5 <- getPostsByTag(tag = 'rhdf5', pages = 3)

posts.me <- getPostsByUser(site = 'bioc', user_id = '3986', n_pages = 20)
posts.me2 <- getPostsByUser(site = 'biostars', user_id = '4156', n_pages = 5)
