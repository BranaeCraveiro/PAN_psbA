library(ggsankey)
library(ggplot2)
library(tidyverse) 
library(patchwork)

setwd("C:\\Local_Files\\SCTLD_samples\\Sample_Data")
colony <- read.csv("PAN-BDT_ColonyData.csv")
samples <- read.csv("PAN-BDT_samples.csv")

#checking column names 
colnames(colony)
colnames(samples)

#add colony id column to both datasets 
colony$colony_id <- paste0(colony$Transect_num, "_", colony$Current_tag_num, "_", colony$Species)
samples$colony_id <- paste0(samples$Transect_num, "_", samples$Current_tag_num, "_", samples$Species)
#check if worked
head(colony)
head(samples)

#keeping colonies within project scope 
colony <- colony[(colony$Species %in% c("CNAT", "ORBI", "OANN", "OFAV", "PSTR", "SSID", "MCAV"))&
                   !(colony$Transect_num %in% c("5", "6")) ,]
samples <- samples[(samples$Sample_type %in% c("Core_EtOH","Core_RNAlater")) &
                         (samples$Species %in% c("CNAT", "ORBI", "OANN", "OFAV", "PSTR", "SSID", "MCAV")) &
                         !(samples$Transect_num %in% c("5", "6")), ]
#check if worked
unique(colony$Species)
unique(colony$Transect_num)
unique(samples$Sample_type)
unique(samples$Species)
unique(samples$Transect_num)

#group OFAV & OANN into ORBI 
colony$Species <- ifelse(
  colony$Species %in% c("OANN", "OFAV"), 
  "ORBI", 
  colony$Species)

samples$Species <- ifelse(
  samples$Species %in% c("OANN", "OFAV"), 
  "ORBI", 
  samples$Species)

#check 
unique(colony$Species)
unique(samples$Species)

#only keep colonys that have at least one sample at some timepoint 
colony <- colony %>%
  filter(colony_id %in% samples$colony_id)
length(unique(colony$colony_id))
length(unique(samples$colony_id))
#113 unique colonies and 123 samples

#renaming health condition columns 

## change colnames into dates 
# list columns with dates
tf <- grepl('Condition', colnames(colony))
date_col <- colnames(colony)[tf]
# get rid of 'condition' to have col names just contain the date
new_col <- gsub("^X", "", date_col)
new_col <- gsub("_Condition", "", new_col)
# convert to date object
formatted_dates <- as.Date(paste0(new_col, "01"), format = "%m%Y%d")
# Format the dates as "Nov 2023", etc.
formatted_labels <- format(formatted_dates, "%b %Y")
# Replace the column names in colony  with the formatted labels
colnames(colony)[tf] <- formatted_labels 

# this is the new list of condiiton columns to use going forward
sample_dates <- c('Sep 2022','Oct 2023','Jul 2024')

#add leading 0 
leading <- sprintf('%06d',unique(samples$Month_year))
# convert to date object
formatted_dates <- as.Date(paste0(leading, "01"), format = "%m%Y%d")
formatted_dates
# Format the dates as "Nov 2023", etc.
formatted_labels <- format(formatted_dates, "%b %Y")
# Set names  
label_lookup <- setNames(formatted_labels, unique(samples$Month_year))
label_lookup
# Replace the month_year values with the formatted dates 
samples$Month_year <- label_lookup[as.character(samples$Month_year)]

# assign factor level in chronological order 
samples$Month_year <- factor(samples$Month_year, levels = sample_dates)
unique(samples$Month_year)


#need to make all conditions into one variable 
lapply(sample_dates, function(col) {
  unique(colony[[col]])})

#turning NAs from 092022 into not yet tagged
colony <- colony %>% 
  replace_na(list("Sep 2022" = "Not yet tagged"))

# replace multiple conditions
colony <- colony %>%
  mutate(across(all_of(sample_dates), ~ case_when(
    # make not visited consistent 
    grepl("Not_visited", ., ignore.case=TRUE) ~ "Not visited",
    # clp & clb -> clb 
    . == "CLP, CLB" ~ "Bleached",
    . == "CLB, CLP" ~ "Bleached",
    . == "CLB,CLP" ~ "Bleached",
    . == "DC, CLP" ~ "Pale",
    . == "CLP, DC" ~ "Pale",
    . == "CLB,CLP " ~ "Bleached",
    . == "CLP " ~ "Pale", 
    . == "Healhty" ~ "Healthy", 
    . == "CLP" ~ "Pale", 
    . == "DC" ~ "Discolored",
    . == "CLB" ~ "Bleached", 
    TRUE ~ .
  )))

#checking if worked 
lapply(sample_dates, function(col) {
  unique(colony[[col]])})

#seperating by colony/species 
sankeydf <- data.frame()
for(id in unique(colony$colony_id)){
  #create dfs for individual colonies 
  df <- colony %>%
    filter(colony_id == id)
  # pivot longer
  sankdf <- df %>% 
    make_long('Sep 2022', 'Oct 2023', 'Jul 2024') %>%
    mutate(colony_id = id)
  # combine into 1 df
  sankeydf <- sankeydf %>%
    bind_rows(sankdf)
}
#checking if worked
head(sankeydf)

#fixing downstream chronological issue - not sure if this actually solved anything 
sankeydf <- sankeydf %>%
  mutate(x = factor(x, levels = sample_dates),
    next_x = factor(next_x, levels = sample_dates))
#check if worked 
str(sankeydf$x)
str(sankeydf$next_x)

# add species labels and sample statuses for each colony id 

# filter samples to match sankeydf format 
sampledf <- samples %>% 
  select(colony_id, Month_year, Health_status) %>%
  rename(x = Month_year,
         sample_condition = Health_status) %>% 
  mutate(x = as.character(x))
#having issue with diff variable types so just as.character both 
sankeydf <- sankeydf %>%
  mutate(x = as.character(x))

# merge
sankey <- sankeydf %>%
  left_join(sampledf,
            # match sample conditions to 'x' (first time point in the set) 
            by = c('colony_id','x')) %>%
  # add species grouping based on colony id 
  rowwise() %>%    
  mutate(species = tail(strsplit(colony_id, "_")[[1]], 1))
#fixing downstream chronoligcal issue 
sankey <- sankey %>%
  mutate(x = factor(x, levels = sample_dates),
    next_x = factor(next_x, levels = sample_dates))

head(sankey,10)

#checking nodes 
unique(sankey$node)

#picking colors
sank_colors = c('Dead'='tomato3',
                'Healthy'='palegreen3',
                'Pale'='lightgoldenrod2',
                'Bleached'='cadetblue1',
                'Discolored' = 'thistle2',
                'Not visited' = 'honeydew2',
                'Not yet tagged' = 'gray78')

# plot all

#for some reason OANN and OFAV appear again, combining into ORBI 
#unique(sankey$species)

#sankey$species <- ifelse(
 # sankey$species %in% c("OANN", "OFAV"), 
 # "ORBI", 
 # sankey$species)
#checking if worked
#unique(sankey$species)

#create species variable for later 
sankey <- sankey %>%
  mutate(species = case_when(
    species == "CNAT" ~ "Colpophyllia natans",
    species == "PSTR" ~ "Pseudodiploria strigosa",
    species == "SSID" ~ "Siderastrea siderea",
    species == "MCAV" ~ "Montastraea cavernosa",
    species %in% c("OFAV", "OANN", "ORBI") ~ "Orbicella spp.",
    .default = species
  ))

# create species variable for later
species_list <- unique(sankey$species)


plots <- list()

for (specie in species_list){
  df <-sankey %>%
    filter(species == specie)
  
  # for each node type, how many samples are there  
  df_count <- df %>%
    group_by(x, node) %>%
    # sum colonies per node & time point (x)
    mutate(colony_count = n(),
           # sum samples per node and time point grouping 
           n_samples = sum(!is.na(sample_condition), na.rm = TRUE),
           label_text = paste0(node, "\n", 
                               colony_count, " colonies\n",
                               n_samples, " samples")
    ) %>% 
    ungroup()
  
  # if condition count != sample count, print colony ID that has NA as sample_condition (wasn't sampled) 
  missing_samples_list<-df_count %>%
    filter(!node %in% c('Dead', 'Not_Visited', 'Not_yet_tagged')) %>%
    filter(colony_count != n_samples & is.na(sample_condition)) %>%
    select(colony_id, x, node)
  # mutate(missing_samples = paste(x,node, sep = " | ")) %>%
  # pull(missing_samples, colony_id)
  
  # plot
  plot <- ggplot(df_count, aes(x = x, next_x = next_x,
                               node = node, next_node = next_node,
                               fill = factor(node))) +
    geom_sankey(flow.alpha = 0.6, node.color = "white", width = 0.1, space = 8) +
    geom_sankey_label(aes(label = label_text), size = 3.0, space = 8) +
    labs(x = NULL) +
    scale_fill_manual("Condition", values = sank_colors) +
    theme_sankey(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.x = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          legend.key.size = unit(0.5, "cm")) +
    ggtitle(paste(specie))
  
  print(plot)
  plots[[specie]] <- plot
  }
  
  # print if there are missing samples
  # if missing_samples tibble has any rows, print them 
  if (nrow(missing_samples_list) > 0) {
    print("Missing samples:")
    print(missing_samples_list)
  }

combined_plot <- wrap_plots(plots, ncol = 3, guides = "collect") &
  coord_cartesian(clip = "off") &
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 2, 5, 2)
  )

ggsave("C:/Local_Files/PAN_psbA/analysis/Plots/sankey_samples.pdf", combined_plot, width = 10, height = 14)
ggsave("C:/Local_Files/PAN_psbA/analysis/Plots/sankey_poster.png", combined_plot, width = 20, height = 10, units = "in", dpi = 300)


  
