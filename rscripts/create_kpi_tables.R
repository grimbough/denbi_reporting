source('get_conda_stats.R')

if (!require("pak", quietly = TRUE))
  install.packages("pak")

packages_required <- c('lubridate', 'BiocPkgTools', 'dplyr', 'openxlsx')

pak::pkg_install(packages_required)

library(lubridate)
library(BiocPkgTools)
library(dplyr)
library(openxlsx)

deNBI_start_year <- 2015
current_year <- year(now())

## A list of relevant de.NBI tools that are available in either bioconductor
## or anaconda
packages <- dplyr::tribble(
  ~pkg, ~in_bioc, 
  'DESeq2', TRUE,
  'biomaRt', TRUE,
  'rhdf5',  TRUE,
  'Rhdf5lib', TRUE,
  'rnaseqGene', TRUE,
  'ComplexHeatmap', TRUE,
  'EBImage', TRUE,
  'SIAMCAT', TRUE,
  'DEXSeq', TRUE,
  'beachmat', TRUE,
  'IHW', TRUE,
  'RnBeads', TRUE,
  'YAPSA', TRUE,
  'IONiseR', TRUE,
  'BiocWorkflowTools', TRUE,
  'EnrichedHeatmap', TRUE,
  'rGREAT', TRUE,
  'HilbertCurve', TRUE,
  'gtrellis', TRUE,
  'motus', FALSE,
  'ngless', FALSE,
  'MOFA', TRUE,
  'MOFA2', TRUE,
  'slalom', TRUE,
  'OmnipathR', TRUE,
  'r-circlize', FALSE,
  'dorothea', TRUE,
  'progeny', TRUE,
  'limix', FALSE)

## If Sina has asked for a specific subset of packages list them here
## Otherwise leave this at NULL
pkgs_required_today <- NULL
# pkgs_required_today <- c(
#   'biomaRt',
#   'DEXSeq',
#   'DESeq2',
#   'BiocWorkflowTools'
# )


## Get the download counts.  
## First two are for Bioconductor directly and for Bioconductor packages obtained from bioconda
## These tables initially have all Bioconductor packages and will be filtered later for the packages we actually want.
## Final table is for non-bioconductor packages
tab <- BiocPkgTools::biocDownloadStats()
tab2 <- BiocPkgTools::anacondaDownloadStats()
tab3 <- getCondaStats(filter(packages, in_bioc == FALSE) %>% pull(pkg), bioc = FALSE)

## Do some wrangling to the three tables so they are in the same format
## We filter() for the relevant date range and packages,
## then select() only the columns we need for our report tables
## The mutate() lines merge together the counts for some tools that are very heavily intertwined.
## e.g. Rhdf5lib and rhdf5 will always be used together
bioc_stats <- tab %>%
  filter(Package %in% packages$pkg, Year >= deNBI_start_year, !is.na(Date)) %>%
  select(-Year, -Month, -pkgType) %>%
  mutate(PackageComb = if_else(grepl(Package, pattern = 'hdf5'), 'rhdf5+Rhdf5lib', Package)) %>%
  mutate(PackageComb = if_else(grepl(PackageComb, pattern = 'MOFA'), 'MOFA+MOFA2', PackageComb))

conda_stats_bioc <- tab2 %>%   
  filter(Package %in% packages$pkg, Year >= deNBI_start_year, !is.na(Date)) %>%
  select(-Year, -Month, -repo) %>%
  mutate(PackageComb = if_else(grepl(Package, pattern = 'hdf5'), 'rhdf5+Rhdf5lib', Package)) %>%
  mutate(PackageComb = if_else(grepl(PackageComb, pattern = 'MOFA'), 'MOFA+MOFA2', PackageComb))

conda_stats_notbioc <- tab3 %>%
  filter(Year >= deNBI_start_year) %>%
  select(-Year, -Month) %>%
  mutate(PackageComb = Package) %>%
  mutate(Nb_of_distinct_IPs = Nb_of_downloads)

## combine the three sets of statistics into a single table
all_stats <- bind_rows(bioc_stats, conda_stats_bioc, conda_stats_notbioc)

## Now we combine the counts for the various sources. 
##  We group by each package and month, then simply sum the download counts.
dl_stats <- all_stats %>% 
  group_by(PackageComb, Date) %>% 
  summarise(Nb_of_downloads = sum(Nb_of_downloads)) %>%
  ungroup() %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  select(PackageComb, Year, Month, Nb_of_downloads)

## Split our single big table into a list of smaller tables, one for each tool
dl_list <- split(dl_stats, dl_stats$PackageComb)


## Final data prettying.
## on each package specific table
## - rename the column to something more descriptive, 
## - select only the years we're interest in
## - remove merged name column - name is now in the overall list
dl_list <- lapply(dl_list, function(x) {
  
  x %>% 
    rename("Monthly_Downloads" = Nb_of_downloads) %>%
    filter(Year >= 2018, Year <= current_year) %>%
    select(-PackageComb)
  
})

## include only the packages we need in the report for Sina
if(!is.null(pkgs_required_today))
  dl_list <- dl_list[ pkgs_required_today ]

#ss <- gs4_create(spring("Huber Tool KPIs - Feb %i", current_year), sheets = dl_list)
write.xlsx(dl_list, file = "tool_KPIs.xlsx")
