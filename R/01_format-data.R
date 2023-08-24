# DESCRIPTION
# This script will load sample metadata and TransectMeasure exports
# This data will be checked for a variety of common errors
# A tidy dataset (*.csv) will be created for use in future scripts

# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(tidyverse)
library(GlobalArchive)

synthesis.name <- "2023_Augusta-SwC"

# Functions ----
# Move this into GlobalArchive package?
ga.read.tm <- function(flnm) {
  read.delim(flnm, header = T, skip = 4, stringsAsFactors = FALSE, colClasses = "character") %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(data.dir ,"/"),"")) %>%
    tidyr::separate(campaign.naming, into = c("campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    dplyr::mutate(campaignid = str_replace_all(.$campaignid,"_[^_]+$", "")) %>%
    ga.clean.names() %>%
    dplyr::select(campaignid, period, image.row, image.col, starts_with("level_"), scientific, 
                  qualifiers, caab_code) %>%
    dplyr::rename(sample = period)
}

# ga.read.tm.test <- function(dir) {
#   list.files(path = data.dir,
#              recursive = T,
#              pattern = "Dot Point Measurements.txt",
#              full.names = T) %>%
#     purrr::map_dfr() 
#   
#   read.delim(flnm, header = T, skip = 4, stringsAsFactors = FALSE, colClasses = "character") %>%
#     dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(data.dir ,"/"),"")) %>%
#     tidyr::separate(campaign.naming, into = c("campaignid"), sep = "/", extra = "drop", fill = "right") %>%
#     dplyr::mutate(campaignid = str_replace_all(.$campaignid,"_[^_]+$", "")) %>%
#     ga.clean.names() %>%
#     dplyr::select(campaignid, period, image.row, image.col, starts_with("level_"), scientific, 
#                   qualifiers, caab_code) %>%
#     dplyr::rename(sample = period)
#    
# }
# 
# habitat <- ga.read.tm.test(dir = data.dir) %>%
#   glimpse()

ga.select.habitat.metadata <- function(data) {
  data %>%
  dplyr::select(campaignid, sample, latitude, longitude, date.time, site, location, status,
                depth, successful.habitat.panoramic, observer.habitat.panoramic) %>%
    dplyr::filter(successful.habitat.panoramic %in% "Yes")
}

ga.read.metadata <- function(dir) {
  list.files(path = dir,
             recursive = T,
             pattern = "_Metadata.csv",
             full.names = T) %>%
    purrr::map_dfr(~read_csv(.))
}

# Set directories for easy use later
data.dir <- "data/raw"

# Load the metadata ----
# This will find all .csv files that end with "_Metadata.csv"
metadata <- ga.read.metadata(dir = data.dir) %>%
  ga.clean.names() %>%
  ga.select.habitat.metadata() %>%
  dplyr::mutate(sample = ifelse(str_length(sample) < 2, 
                                str_pad(sample, width = 2, side = "left", pad = "0"), sample)) %>%
  glimpse()

# Check for errors in the metadata ----
# Check for blanks in any of the columns
if (length(names(which(colSums(is.na(metadata)) > 0))) == 0) {
  message("You are not missing any required values in your metadata!")
} else {
  message(paste("You are missing values in the required columns", 
                toString(names(which(colSums(is.na(metadata)) > 0))), sep = ": "))
}

# If no errors then continue - otherwise you need to fix the errors in the source data!

# Load the raw TransectMeasure annotation data ----
habitat <- list.files(path = data.dir,
                      recursive = T,
                      pattern = "Dot Point Measurements.txt",
                      full.names = T) %>%
  purrr::map_dfr(~ga.read.tm(.)) %>%
  glimpse()

# Check for errors in the raw TransectMeasure data ----
# Check number of points per sample
n.points <- 80 # Set number of points for your campaign

wrong.no.points <- habitat %>%
  group_by(campaignid, sample) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n != n.points) %>%
  glimpse()

# Check points that haven't been annotated
missed.annotation.points <- habitat %>%
  dplyr::filter(level_2 %in% "") %>%
  glimpse()

# Check annotations that don't have a match in metadata
missing.metadata <- habitat %>%
  distinct(campaignid, sample) %>%
  anti_join(metadata) %>%
  glimpse()

# Check for samples that don't have any annotation data
missing.samples <- metadata %>%
  distinct(campaignid, sample) %>%
  anti_join(habitat) %>%
  glimpse()

# Check for annotations that don't match the accepted schema ----
# Load the schema
schema <- read.delim("data/raw/benthic.habitat.annotation.schema.forward.facing.20230405.150927.txt",
                     colClasses = "character") %>%
  glimpse()

# Check for annotation points with attributes that don't match the accepted schema
wrong.schema <- habitat %>%
  anti_join(schema)

# Check for annotation points that have a blank caab code
missing.caab <- habitat %>%
  dplyr::filter(caab_code %in% "") %>%
  glimpse()

# Check for CAAB codes with attributes that don't match with the accepted schema
wrong.caab <- habitat %>%
  distinct(pick(caab_code, starts_with("level_"))) %>%
  anti_join(schema)

# GO BACK AND FIX YOUR ERRORS IN THE SOURCE DATA (EG THE .TMobs FILES) !!!

# Export data to a tidy .csv file for use in subsequent scripts ----
tidy.habitat <- habitat %>%
  dplyr::filter(!level_2 %in% "") %>%                                           # This should do nothing if you have rectified errors in your original data
  dplyr::filter(!level_2 %in% c("Unscorable", "Fishes", "Molluscs")) %>%
  dplyr::mutate(broad = case_when(level_2 %in% "Sponges" ~ "sessile.invertebrates",
                                  level_2 %in% "Sessile invertebrates" ~ "sessile.invertebrates",
                                  level_2 %in% "Bryozoa" ~ "sessile.invertebrates",
                                  level_2 %in% "Cnidaria" ~ "sessile.invertebrates",
                                  level_2 %in% "Macroalgae" ~ "macroalgae",
                                  level_2 %in% "Substrate" & level_3 %in% "consolidated (hard)" ~ "Rock",
                                  level_2 %in% "Substrate" & level_3 %in% "unconsolidated (soft)" ~ "Sand",
                                  level_2 %in% "Seagrasses" ~ "seagrasses"),
                count = 1) %>%
  dplyr::select(-c(starts_with("level_"), scientific, qualifiers, caab_code)) %>%
  pivot_wider(names_from = broad, values_from = count, values_fill = 0) %>%
  dplyr::select(-c(image.row, image.col)) %>%
  group_by(campaignid, sample) %>%
  summarise_all(list(sum)) %>%
  ungroup() %>%
  mutate(total.points.annotated = rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>%
  left_join(metadata) %>%
  glimpse()

write.csv(tidy.habitat, file = paste0("data/tidy/", synthesis.name, "_habitat.csv"),
          row.names = F)
