setwd("C:\\Users\\Owner\\OneDrive\\Documents\\GW Lab\\PAN\\sequences")

files <- list.files(pattern = ".ab1")                #only chromatagraphy files 
name_map <- read.csv("Sequencing_Metadata.csv")

#naming file loop:
for (i in 1:nrow(name_map)) {
  
  barcode <- name_map$Barcode[i]
  new_name <- paste0(name_map$File_Name[i], ".ab1")
  
  barcode_match <- files[grepl(barcode, files)]
  
  #checks 
  #rename file only if there is one match and the new file doesn't exist already
  if (length(barcode_match) == 1 && !file.exists(new_name)) {
    file.rename( barcode_match, new_name)
  }}