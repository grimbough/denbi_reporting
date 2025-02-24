source('support_stats.R')
library(BiocPkgTools)
library(dplyr)

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

## these are the packages Sina has asked for
pkgs_required_today <- c(
  'biomaRt',
  'DEXSeq',
  'DESeq2',
  'BiocWorkflowTools',
  'rnaseqGene',
  'rhdf5+Rhdf5lib',
  'EBImage',
  'IHW',
  'SIAMCAT',
  'r-circlize',
  'ComplexHeatmap',
  'EnrichedHeatmap',
  'rGREAT',
  'HilbertCurve',
  'gtrellis',
  'MOFA+MOFA2',
  'slalom',
  'limix'
)

fct_levels <- packages %>% 
  mutate(PackageComb = if_else(grepl(pkg, pattern = 'hdf5'), 'rhdf5+Rhdf5lib', pkg)) %>%
  mutate(PackageComb = if_else(grepl(PackageComb, pattern = 'MOFA'), 'MOFA+MOFA2', PackageComb)) %>%
  magrittr::extract2("PackageComb") %>% unique()

tab <- BiocPkgTools::biocDownloadStats()
tab2 <- BiocPkgTools::anacondaDownloadStats()
tab3 <- getCondaStats(filter(packages, in_bioc == FALSE) %>% pull(pkg), bioc = FALSE)

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

## combine the two sets of anaconda statistics into a single table
conda_stats <- bind_rows(conda_stats_bioc, conda_stats_notbioc)


dl_stats <- bind_rows(bioc_stats, conda_stats) %>% 
  group_by(PackageComb, Date) %>% 
  summarise(Nb_of_downloads = sum(Nb_of_downloads)) %>%
  ungroup() %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  select(PackageComb, Year, Month, Nb_of_downloads)

dl_list <- split(dl_stats, dl_stats$PackageComb)


## on each package specific table
## - rename the column, 
## - select only the years we're interest in
## - remove merge name column - name is now in the overall list
dl_list <- lapply(dl_list, function(x) {
  
  x %>% 
    rename("Monthly_Downloads" = Nb_of_downloads) %>%
    filter(Year >= 2018, Year < 2025) %>%
    select(-PackageComb)
  
})

## include only the packages we need in the report for Sina
dl_list <- dl_list[ pkgs_required_today ]

ss <- gs4_create(spring("Huber Tool KPIs - Feb %i", current_year), sheets = dl_list)

