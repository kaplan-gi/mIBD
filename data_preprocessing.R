# Title: data pre-processing for the mIBD Global Systematic Review Shiny Application
# Contributor: Julia Gorospe
# Created: 2026-01-27
# Updated: 2026-06-17
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1

# converts the REDCap data output to a format that can be input into the Shiny app


# load libraries
library(tidyverse)
library(sf)
library(geojsonsf)
library(seqinr) # amino acid codes



# load data
cols <- as.character(seq(1,346)) 
data <- read_csv(
  "MIBDMappingProject_DATA_LABELS_2026-07-09_0508.csv", 
  col_names = cols,
  skip = 1
)

# select columns and rename
col_names <- read_csv("col_names.csv") %>% 
  filter(!is.na(new_col_name))

data <- data %>% 
  select(as.character(col_names$col)) %>% 
  set_names(col_names$new_col_name)

# filter for rows with variant information

# cleaning the Mutation - Protein columns
# converting 3 letter amino acid codes to 1 letter amino acid codes
hgvs3to1 <- function(x) {
  
  aa3 <- c("Ala","Arg","Asn","Asp","Cys","Gln","Glu","Gly",
           "His","Ile","Leu","Lys","Met","Phe","Pro","Ser",
           "Thr","Trp","Tyr","Val")
  
  aa1 <- a(aa3)
  names(aa1) <- aa3
  
  # Replace every 3-letter amino acid with its 1-letter equivalent
  for (i in seq_along(aa3)) {
    x <- gsub(aa3[i], aa1[i], x, fixed = TRUE)
  }
  
  # HGVS uses Ter; convert to *
  x <- gsub("Ter", "*", x, fixed = TRUE)
  x <- gsub("X", "*", x, fixed = TRUE)
  
  x
}

data <- data %>% 
  
  # remove rows with no variant data
  filter(!is.na(dna_change) | !is.na(rna_change) | !is.na(aa_change)) %>% 
  
  # append the heterozygous variants
  pivot_longer(
      .,
      cols = c(aa_change, aa_change_2),
      names_to = "heterozygous",
      values_to = "aa_change",
      values_drop_na = FALSE) %>% 
  
  # clean protein change columns
  mutate(aa_change = str_remove_all(aa_change, "^.*?:|p\\.|,|\\(|\\)|\\s.*$")) %>% 
  mutate(aa_change = paste0("p.", aa_change)) %>% 
  mutate(aa_change = hgvs3to1(aa_change)) %>% 
  mutate(aa_change = ifelse(grepl("^p\\.[A-Z][0-9]", aa_change), aa_change, NA)) %>% 
  
  # remove commas so the lollipop chart can read as MAF
  mutate(across(everything(), ~ str_replace_all(., ",", ";"))) %>% 
  
  # recode the variant_type column so it is recognizable
  mutate(variant_type = case_when(variant_type == "Frameshift variant" ~ "Frame_Shift",
                                  variant_type == "Nonsense (stop-gain) variant" ~ "Nonsense_Mutation",
                                  variant_type == "Missense variant" ~ "Missense_Mutation",
                                  variant_type == "Splice-site variant (donor or acceptor)" ~ "Splice_Site",
                                  variant_type == "Deletion" ~ "In_Frame_Del",
                                  TRUE ~ "Other")) %>% 
  
  # recode the disease type column
  mutate(IBD_subtype = case_when(IBD_subtype == "Crohn's Disease" ~ "CD",
                                 IBD_subtype == "Ulcerative Colitis" ~ "UC",
                                 IBD_subtype == "Crohn's like disease" ~ "CD-like",
                                 TRUE ~ IBD_subtype)) # move to preprocessing
  
data_wide <- data %>% 
  pivot_wider(
    .,
    names_from = heterozygous,
    values_from = aa_change
  )
# url formatting



# load geojson
geo <- geojson_sf("res50m_230223.geojson") %>% 
  select(name, admin, geometry)

# check overlap in country names between the datasets
setdiff(data$country, geo$admin)

# add country polygons to the data
data_geo <- data_wide %>% 
  mutate(country = if_else(country == "United States", "United States of America", country)) %>% 
  left_join(., geo, by = join_by("country" == "admin"))



# write out 2 datasets
write_csv(data, "mIBD_data.csv", na = "")
st_write(data_geo, "mIBD_data_geo.geojson")
