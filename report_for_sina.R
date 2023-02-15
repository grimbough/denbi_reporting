source('support_stats.R')
library(BiocPkgTools)
library(dplyr)

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
  'slalom', TRUE,
  'OmnipathR', TRUE,
  'r-circlize', FALSE,
  'dorothea', TRUE,
  'progeny', TRUE)


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
  'gtrellis'
)

fct_levels <- packages %>% 
  mutate(PackageComb = if_else(grepl(pkg, pattern = 'hdf5'), 'rhdf5+Rhdf5lib', pkg)) %>%
  magrittr::extract2("PackageComb") %>% unique()

tab <- BiocPkgTools::biocDownloadStats()
tab2 <- BiocPkgTools::anacondaDownloadStats()
tab3 <- getCondaStats(filter(packages, in_bioc == FALSE) %>% pull(pkg), bioc = FALSE)

bioc_stats <- tab %>%
  filter(Package %in% packages$pkg, Year >= 2015, !is.na(Date)) %>%
  select(-Year, -Month, -pkgType) %>%
  mutate(PackageComb = if_else(grepl(Package, pattern = 'hdf5'), 'rhdf5+Rhdf5lib', Package))

conda_stats_bioc <- tab2 %>%   
  filter(Package %in% packages$pkg, Year >= 2015, !is.na(Date)) %>%
  select(-Year, -Month, -repo) %>%
  mutate(PackageComb = if_else(grepl(Package, pattern = 'hdf5'), 'rhdf5+Rhdf5lib', Package))

conda_stats_notbioc <- tab3 %>%
  filter(Year >= 2015) %>%
  select(-Year, -Month) %>%
  mutate(PackageComb = Package) %>%
  mutate(Nb_of_distinct_IPs = Nb_of_downloads)

conda_stats <- bind_rows(conda_stats_bioc, conda_stats_notbioc)


dl_stats <- bind_rows(bioc_stats, conda_stats) %>% 
  group_by(PackageComb, Date) %>% 
  summarise(Nb_of_downloads = sum(Nb_of_downloads)) %>%
  ungroup() %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  select(PackageComb, Year, Month, Nb_of_downloads)

dl_list <- split(dl_stats, dl_stats$PackageComb)

dl_list <- lapply(dl_list, function(x) {
  
  x %>% 
    rename("Monthly_Downloads" = Nb_of_downloads) %>%
    filter(Year >= 2018, Year < 2023) %>%
    select(-PackageComb)
  
})

dl_list <- dl_list[ pkgs_required_today ]

ss <- gs4_create("Huber Tool KPIs - Feb 2023", sheets = dl_list)

