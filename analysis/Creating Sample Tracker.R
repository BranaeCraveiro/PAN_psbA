#load libraries
library(tidyverse)
library(dplyr)
library(data.table)

setwd("C:\\Local_Files\\SCTLD_samples\\Sample_Data")

#load colony data
colony <- read.csv("PAN-BDT_ColonyData.csv")

#step 1: cleaning colony data 

#checking headers 
head(colony)

#removing MMEAs (breaks future merge look at lines 150-154)
colony <- colony[!colony$Species == "MMEA", ]
                                     
#keeping useful columns 
colony_clean=colony[c('Transect_num', 'Current_tag_num', 'Species', 'X092022_Condition', 
                      'X102023_Condition', 'X072024_Condition')]

#renaming health condition columns 
colony_clean <- colony_clean %>% 
  rename_with(
    .cols = matches ("^X\\d{6}_Condition$"), 
    .fn = ~ str_extract(., "\\d{6}")
  ) 

#putting df in long format
colony_long <- colony_clean %>% 
  pivot_longer(
    cols = matches ("^\\d{6}"), 
    names_to = "MonthYear", 
    values_to = "Condition"
  )

#add colony column 
colony_long$colony = c(paste0(colony_long$Transect_num, "_", colony_long$Current_tag_num))

#checking if code worked
#want to see 5 columns: transect num, current tag num, monthyear, condition, and colony 
head(colony_long)

#removing transcet num and current tag number columns now that we have the colony column 
colony_long$Transect_num =NULL
colony_long$Current_tag_num =NULL

#checking if worked 
head(colony_long)

#Step 2: clean sample data 

#load Sample_data.csv
sample=read.csv("PAN-BDT_samples.csv")

#adding a colony column to sample data
sample$colony=colony=c(paste0(sample$Transect_num, "_", sample$Current_tag_num))

#only use EtOH and RNALater samples & target species, exclude nontarget transects
sample_DNA <- sample[(sample$Sample_type %in% c("Core_EtOH","Core_RNAlater")) &
                       (sample$Species %in% c("CNAT", "ORBI", "OANN", "OFAV", "PSTR", "SSID", "MCAV"))&
                       !(sample$Transect_num %in% c("5", "6")) ,]

#checking if code worked (want to see last column as colony) 
head(sample_DNA)

#Step 3: cleaning metagenomics df 

#load Metagenomics_tracker.csv
metagenomics=read.csv("Metagenomics_Tracker_PAN-BDT.csv")

#checking df 
head(metagenomics)

#changing X column to Tubelabel_species
names(metagenomics)[names(metagenomics) == 'X'] <- 'Tubelabel_species'

#checking if column name changed 
colnames(metagenomics)

#isolating useful columns 
metagenomics_PCR = metagenomics[c("Tubelabel_species", "Health_Status", "Extracted", "Raw_ng_ul", "Extraction_physical_location" , "Notes")]

#checking if code worked
head(metagenomics_PCR)

#renaming notes column
names(metagenomics_PCR)[6] <- "Extraction_notes"
#check if worked 
head(metagenomics_PCR)


#Step 4: merging sample_DNA and metagenonics_PCR into one dataframe 

#need to make number of rows match - need to remove samples not yet extracted
nrow(metagenomics_PCR)
nrow(sample_DNA)


#keeping only my extracted samples (ones in penguin fridge) 
sample_DNA <- sample_DNA[!is.na(sample_DNA$Extraction_physical_location) &
                           !(sample_DNA$Transect_num %in% c("5", "6")) &
                           (sample_DNA$Extraction_physical_location %in% 
                              c("UML_PENGUIN_B1","UML_PENGUIN_B2", "UML_PENGUIN_B3", "UML_NARWHAL_R9_B2")) ,]
                        
metagenomics_PCR <- metagenomics_PCR[!is.na(metagenomics_PCR$Extraction_physical_location) &
                                       (metagenomics_PCR$Extraction_physical_location %in% 
                                          c("UML_PENGUIN_B1","UML_PENGUIN_B2", "UML_PENGUIN_B3", "UML_NARWHAL_R9_B2")) &
                                       !grepl("T5|T6", metagenomics_PCR$Tubelabel_species) ,]

#for future reference some reason NAs and blanks were messing with the filtering so I had to clean those also 

head(sample_DNA)
head(metagenomics_PCR)

nrow(metagenomics_PCR)
nrow(sample_DNA)

#troubleshooting why rows counts are not adding up   
#manually checked the metagenomics tracker and that has the correct number of extracted samples

#rows in meta that are not in sample
(metagenomics_PCR[!(metagenomics_PCR$Tubelabel_species %in% sample_DNA$Tubelabel_species), ])
#forgot to add ORBI, OANN, & OFAV when filtering sample data earlier oops

#rows in sample that are not in meta
(sample_DNA[!(sample_DNA$Tubelabel_species %in% metagenomics_PCR$Tubelabel_species), ])
#IMPORTANT: the tube label species are different for T4 samples (accidentally relabeled from T4 to T5 on irl tubes)

sum(!(metagenomics_PCR$Tubelabel_species %in% sample_DNA$Tubelabel_species))

table(sample_DNA$Species, sample_DNA$Extraction_physical_location)

#I extracted one sample twice by accident so I need to remove the extra from meta_PCR
print(metagenomics_PCR[(metagenomics_PCR$Tubelabel_species == "92022_PAN_BDT_T2_48_CNAT"), ]) 

metagenomics_PCR <- metagenomics_PCR[!(metagenomics_PCR$Tubelabel_species == "92022_PAN_BDT_T2_48_CNAT" & 
                                         metagenomics_PCR$Extracted == "5_23_2025"), ]
#checking if worked 
nrow(metagenomics_PCR)
nrow(sample_DNA)
print(metagenomics_PCR[(metagenomics_PCR$Tubelabel_species == "92022_PAN_BDT_T2_48_CNAT"), ]) 

#info using to create ties between sample_dna and metagenomics_PCR
sample_DNA=sample_DNA[c("Month_year", "Tubelabel_species", "colony")]

sample_tracker <- merge(x=metagenomics_PCR, y=sample_DNA, by="Tubelabel_species")

#fixing month year column so the names match 
names(sample_tracker)[7] <- "MonthYear"
colnames(sample_tracker)

#having issues with the merging sample tracker and colony data where it is
#adding an extra row so checking if there is a duplicate colony code in colony data 
print(colony_long[duplicated(colony_long[, c("MonthYear", "colony")]), ])

#there is also an MMEA that is colony 3_80, removing it earlier in my code since I am not looking at MMEAs

#having issue with condition inputting a bunch of NAs so looking into that now 

#rows in sample tracker that are not in colony
(sample_tracker[!(sample_tracker$colony %in% colony_long$colony), ])
#rows in colony that are not in sample tracker 
(colony_long[!(colony_long$colony %in% sample_tracker$colony), ])

#looking at the two df it seems to be a leading 0 in the MonthYear that is causing the mismatch
sample_tracker$MonthYear <- as.character(sample_tracker$MonthYear)
sample_tracker$MonthYear <- ifelse(nchar(sample_tracker$MonthYear) == 5, paste0("0", sample_tracker$MonthYear), sample_tracker$MonthYear)
head(sample_tracker$MonthYear)

#merging sample tracker and colony data
sample_tracker <- merge(sample_tracker, colony_long, by = c("MonthYear", "colony"), all.x = TRUE)

write.csv(sample_tracker, "psbA_sample_tracker.csv", row.names = FALSE)

