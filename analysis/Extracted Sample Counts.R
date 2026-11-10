library(dplyr)
library(tidyverse)
library(knitr)

setwd("C:\\Local_Files\\SCTLD_samples\\Sample_Data")
samples <- read.csv("PAN-BDT_samples.csv")

#checking col names & row counts
colnames(samples)
nrow(samples)

#checking options w/in columns 
unique(samples$Sample_type)
unique(samples$Species)

#only use EtOH and RNALater samples & target species, exclude nontarget transects
pan_samples <- samples[(samples$Sample_type %in% c("Core_EtOH", "Core_RNAlater")) &
                             !(samples$Species %in% c("MMEA", "DLAB", "PAST", "AS-SINT"))&
                             !(samples$Transect_num %in% c("5", "6")) ,]

#checking if worked 
unique(pan_samples$Transect_num)
unique(pan_samples$Species)
nrow(pan_samples)

#need to get rid of duplicate samples 
pan_samples <- pan_samples %>% 
  distinct(Month_year, Transect_num, Current_tag_num , Species, Health_status, .keep_all = TRUE)
#check
nrow(pan_samples)

#group ORBI & OANN into ORBI 
pan_samples$Species <- ifelse(
  pan_samples$Species %in% c("OANN", "OFAV"), 
  "ORBI", 
  pan_samples$Species)

#counting how many samples per species
sample_counts <- pan_samples %>% count(Species)
#species counts markdown (for obsidian)
kable(sample_counts, format = "markdown")

#there are less samples then there should be so will compared extracted CNAT counts
samples_extracted <-samples[(samples$Sample_type %in% c("Core_EtOH", "Core_RNAlater")) &
                              !(samples$Species %in% c("MMEA", "DLAB", "PAST", "AS-SINT"))&
                              !(samples$Transect_num %in% c("5", "6")) & 
                              (samples$Extraction_physical_location %in% 
                                 c("UML_PENGUIN_B1","UML_PENGUIN_B2", "UML_PENGUIN_B3", "UML_NARWHAL_R9_B2")) ,]
                            
#checking if worked 
nrow(samples_extracted)
unique(samples_extracted$Transect_num)
unique(samples_extracted$Species)

#group ORBI & OANN into ORBI 
samples_extracted$Species <- ifelse(
  samples_extracted$Species %in% c("OANN", "OFAV"), 
  "ORBI", 
  samples_extracted$Species)

#counting how many samples per species
extracted_counts <- samples_extracted %>% count(Species)
#species counts markdown (for obsidian)
kable(extracted_counts, format = "markdown")

#YAY THEY MATCH FOR CNATs! distinct function worked how I wanted it to!!! 