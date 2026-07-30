# Title: mIBD Global Systematic Review Shiny Application, global with packages and data
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2026-01-27
# Updated: 2026-07-30
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1


# load libraries
library(rsconnect) # version 0.8.18
library(shiny) # version 1.6.0
library(shinyjs) # show/hide function
library(bslib)
library(htmlwidgets)
library(shinyWidgets)
library(readr) # version 1.4.0; parsing date information
library(tidyverse) # version 0.18
library(shinycssloaders)
library(leaflet) # version 2.0.4.1
library(sf)
library(geojsonsf)
library(plotly)
library(g3viz)
library(DT)
library(fontawesome)


# load datasets
# path <- "~/Dropbox/COVID_ShinyApp/mIBD/" # Julia's development path
path <- "https://raw.githubusercontent.com/kaplan-gi/mIBD/main/"

geo <- geojson_sf(paste0(path, "mIBD_data_geo.geojson"))

data <- read_csv(paste0(path, "mIBD_data.csv")) %>% 
    mutate(IBD_subtype = case_when(IBD_subtype == "Crohn's Disease" ~ "CD",
                                   IBD_subtype == "Ulcerative Colitis" ~ "UC",
                                   IBD_subtype == "Crohn's like disease" ~ "CD-like",
                                   TRUE ~ IBD_subtype)) # move to preprocessing

data_subset <- geo %>% filter(name %in% data$country) %>% 
  merge(., data, by.x = "name", by.y = "country") # move to preprocessing



# generate datasets 

# row = case (data)

# row = country summary (map)
country_data <- data_subset %>% 
  group_by(name) %>% 
  summarize(cases = n(),
            genes = length(unique(gene_name))) %>% 
  ungroup()

# row = variant (append)

# row = therapy (pivot)
data_tre <- data %>% 
  select(record_number, DOI, gene_name, starts_with("tre_")) %>% 
  pivot_longer(cols = starts_with("tre_"),
               names_to = "tr_type",
               names_prefix = "tre_",
               values_to = "tre_bin") %>% 
  filter(tre_bin == "Checked") %>% 
  group_by(gene_name, tr_type) %>% 
  summarize(tre_count = n())

data_trne <- data %>% 
  select(record_number, DOI, gene_name, starts_with("trne_")) %>% 
  pivot_longer(cols = starts_with("trne_"),
               names_to = "tr_type",
               names_prefix = "trne_",
               values_to = "trne_bin") %>% 
  filter(trne_bin == "Checked") %>% 
  group_by(gene_name, tr_type) %>% 
  summarize(trne_count = n())

data_tr <- data %>% 
  select(record_number, DOI, gene_name, starts_with("tr_")) %>% 
  pivot_longer(cols = starts_with("tr_"),
               names_to = "tr_type",
               names_prefix = "tr_",
               values_to = "tr_bin") %>% 
  filter(tr_bin == "Checked") %>% 
  group_by(gene_name, tr_type) %>% 
  summarize(tr_count = n()) %>%  
  left_join(., data_tre, by = c("gene_name", "tr_type")) %>% 
  left_join(., data_trne, by = c("gene_name", "tr_type"))

# row = eic (pivot)
data_eic <- data %>% 
  select(record_number, gene_name, starts_with("eic_")) %>% 
  pivot_longer(cols = starts_with("eic_"),
               names_to = "eic_type",
               names_prefix = "eic_",
               values_to = "eic_bin") %>% 
  mutate(eic_bin = case_when(eic_bin == "Checked" ~ 1,
                            eic_bin == "Unchecked" ~ 0,
                            TRUE ~ NA))
  #filter(eic_bin == "Checked")



# format data for download


# assign colors to countries for consistency (colourblind friendly tol_muted palette)
plot_pal <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#D3BD4E', '#999933','#CC6677', '#882255', '#AA4499', '#363538')
map_pal <- colorNumeric(palette = c("#DBFEF4", "#002B1E"), domain = range(country_data$cases, na.rm = TRUE))


# random plot designs
bar_genes <- function(df){
  df %>% 
    group_by(gene_name) %>% 
    summarise(case_count = n()) %>% 
    plot_ly() %>%
    add_trace(x = ~gene_name, y = ~case_count,
              type = "bar",
              marker = list(color = "#0b8964"),
              hoverinfo = "skip",
              showlegend = FALSE) %>%
    layout(title = "<b>Gene Frequency</b>",
           xaxis = list(
             title = "",
             categoryorder = "total descending"
             ),
           yaxis = list(title = list(
             text = "Case Count",
             standoff = 15L),
             tickformat = ".0f",
             dtick = 1
           ),
           margin = list(t = 50, r = 0, b = 10, l = 0, pad = 4),
           font = list(family = "Ubuntu")) %>% 
    config(displayModeBar = FALSE)
}


# text-heavy HTML sections
definitions <- '<div class="glossary">
  <p><strong>mIBD (monogenic inflammatory bowel disease):</strong> A form of inflammatory bowel disease tracible to changes in a single gene which often presents in early childhood.</p>
  
  <p><strong>Phenotypes of IBD:</strong> The observable clinical characteristics used to describe different forms of inflammatory bowel disease</p>
  
  <p><strong>CD (Crohn’s disease):</strong> A chronic inflammatory bowel disease characterized by inflammation that can affect any part of the gastrointestinal tract, often involving patchy, transmural (full-thickness) inflammation and complications such as strictures or fistulas.</p>
  
  <p><strong>UC (ulcerative colitis):</strong> A chronic inflammatory bowel disease limited primarily to the colon and rectum, characterized by continuous inflammation of the colonic mucosa typically beginning in the rectum.</p>
  
  <p><strong>IBDU (inflammatory bowel disease unclassified):</strong> A diagnosis used when a patient has features of inflammatory bowel disease but the clinical, endoscopic, or histological findings are insufficient to clearly classify the disease as Crohn’s disease or ulcerative colitis.</p>
  
  <p><strong>Crohn’s-like:</strong> A disease presentation with clinical, endoscopic, or histological features resembling Crohn’s disease, but which may be caused by another underlying condition (e.g., intestinal Behçet disease or granulomatous colitis).</p>
  
  <p><strong>Gene:</strong> A segment of DNA that contains instructions for producing a functional product such as a protein.</p>
  
  <p><strong>Mutation:</strong> A permanent change to a DNA sequence that may alter gene function. Depending on its effect, a mutation can be harmless, contribute to disease risk, or directly cause disease.</p>
  
  <p><strong>Variant:</strong> A specific difference in a DNA sequence when compared with a reference genome.</p>
  
  <p><strong>Variant type:</strong> The classification of a genetic change based on the type of DNA alteration (e.g. single nucleotide variant, insertion, deletion, duplication).</p>
  
  <p><strong>Autosomal recessive:</strong> A pattern of inheritance in which a disease-causing variant must be present in both copies of an autosomal gene (one inherited from each parent) for the condition to occur.</p>
  
  <p><strong>X-linked recessive:</strong> A pattern of inheritance where a disease-causing variant is located on the X chromosome, typically affecting individuals with one X chromosome more severely because they lack a second copy of the gene.</p>
  
  <p><strong>Autosomal dominant:</strong> A pattern of inheritance in which a disease-causing variant in only one copy of an autosomal gene is sufficient to cause the condition.</p>
  
  <p><strong>Compound heterozygous:</strong> A pattern of inheritance in which the individual inherits two different altered alleles for the same gene.</p>
  
  <p><strong>Sequencing techniques:</strong> Laboratory methods used to determine the order of DNA bases in genetic material to identify genetic variants associated with disease.</p>
  
  <p><strong>NGS (next-generation sequencing):</strong> A high-throughput DNA sequencing technology that allows many genes or large portions of the genome to be analyzed simultaneously.</p>
  
  <p><strong>WES (whole-exome sequencing):</strong> A sequencing method that examines the protein-coding regions of genes (the exome).</p>
  
  <p><strong>TGPS (targeted gene-panel sequencing):</strong> A sequencing approach that analyzes a selected group of genes known to be associated with a specific disease or group of conditions.</p>
  
  <p><strong>WGS (whole-genome sequencing):</strong> A sequencing method that analyzes both the coding and non-coding DNA regions of the entire genome.</p>
  
  <p><strong>Extraintestinal Comorbidity:</strong> A medical condition occurring outside the gastrointestinal tract but is associated with inflammatory bowel disease. Examples may include joint, skin, eye, liver, immune, or growth-related complications.</p>
  </div>'

# options
options(spinner.color="#52D6F4")#, warn = -1)




  
