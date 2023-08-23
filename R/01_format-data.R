# DESCRIPTION
# This script will load sample metadata and TransectMeasure exports
# This data will be checked for a variety of common errors
# A tidy dataset (*.csv) will be created for use in future scripts

# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(tidyverse)
library(GlobalArchive)
library(beepr) # Just for fun

# Functions ----
# Move this into GlobalArchive package?
read_tm_delim <- function(flnm) {
  read.delim(flnm, header = T, skip = 4, stringsAsFactors = FALSE, colClasses = "character") %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(data.dir ,"/"),"")) %>%
    tidyr::separate(campaign.naming, into = c("campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    dplyr::mutate(campaignid = str_replace_all(.$campaignid,"_[^_]+$", "")) %>%
    ga.clean.names() %>%
    dplyr::select(campaignid, period, image.row, image.col, starts_with("level_"), scientific, 
                  qualifiers, caab_code) %>%
    dplyr::rename(sample = period)
}

# Set directories for easy use later
data.dir <- "data/raw"

# Load the metadata ----
# This will find all .csv files that end with "_Metadata.csv"
metadata <- list.files(path = data.dir,
                       recursive = T,
                       pattern = "_Metadata.csv",
                       full.names = T) %>%
  purrr::map_dfr(~read_csv(.)) %>%
  ga.clean.names() %>%
  dplyr::select(campaignid, sample, latitude, longitude, date.time, site, location, status,
                depth, successful.habitat.panoramic, observer.habitat.panoramic) %>%
  dplyr::filter(successful.habitat.panoramic %in% "Yes") %>%
  dplyr::mutate(sample = ifelse(str_length(sample) < 2, 
                                str_pad(sample, width = 2, side = "left", pad = "0"), sample)) %>%
  glimpse()

# Check for errors in the metadata ----
# Check for blanks in any of the columns
if (length(names(which(colSums(is.na(metadata)) > 0))) == 0) {
  beep(8)
  message("You are not missing any required values in your metadata!")
} else {
  beep(2)
  message(paste("You are missing values in the required columns", 
                toString(names(which(colSums(is.na(metadata)) > 0))), sep = ": "))
}

# If no errors then continue - otherwise you need to fix the errors in the source data!

# Load the raw TransectMeasure annotation data ----
habitat <- list.files(path = data.dir,
                      recursive = T,
                      pattern = "Dot Point Measurements.txt",
                      full.names = T) %>%
  purrr::map_dfr(~read_tm_delim(.)) %>%
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
