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

getGithubIssues <- function(user = 'grimbough', repo = "rhdf5") {
    
    url <- paste0('https://api.github.com/repos/', user, 
                  '/', repo, '/issues?state=all')
    
    json <- content(GET(url))
    creation_date <- vapply(json, function(x) return(x$created_at), character(1)) %>%
        as.Date()
    
    res <- tibble(creation_date = creation_date, tag = repo, site = "github")
    return(res)
}

all_posts <- bind_rows(getPostsByTag(tag = 'biomaRt', site = "bioc", n_pages = 8), 
                           getPostsByTag(tag = 'biomaRt', site = 'biostars', n_pages = 6),
                           getPostsByTag(tag = 'rhdf5+Rhdf5lib', site = "bioc", n_pages = 4), 
                           getPostsByTag(tag = 'rhdf5+Rhdf5lib', site = 'biostars', n_pages = 1),
                           getPostsByTag(tag = 'IONiseR', site = "bioc", n_pages = 3), 
                           getPostsByTag(tag = 'IONiseR', site = 'biostars', n_pages = 1))

github_issues <- lapply(c('rhdf5', 'Rhdf5lib', 'biomaRt', 'BiocWorkflowTools', 'IONiseR'),
                        getGithubIssues, user = 'grimbough') %>%
    bind_rows() %>% 
    mutate(tag = if_else(grepl(tag, pattern = 'hdf5'), 'rhdf5+Rhdf5lib', tag))

all_posts %>% 
    bind_rows(github_issues) %>%
    filter(year(creation_date) >= 2015) %>% 
    ggplot(aes(fill = site, x = year(creation_date))) + 
    geom_bar() +
    scale_fill_manual(values = c('#1a81c2', '#8f2c47', 'grey40'),
                      name = "Site",
                      labels = c("bioconductor.org", "biostars.org", "github.com")) + 
    theme_bw() + 
    xlab('Year') +
    ylab('No. of Questions') +
    facet_wrap(~ tag) 
    

posts.me <- getPostsByUser(site = 'bioc', user_id = '3986', n_pages = 20)
posts.me2 <- getPostsByUser(site = 'biostars', user_id = '4156', n_pages = 5)



read.delim('https://www.bioconductor.org/packages/stats/bioc/Rhdf5lib/Rhdf5lib_stats.tab')
