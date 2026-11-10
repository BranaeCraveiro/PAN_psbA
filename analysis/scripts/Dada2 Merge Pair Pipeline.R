library(dada2)

path <- setwd("C:\\Users\\Owner\\OneDrive\\Documents\\GW Lab\\PAN\\sequences")
list.files(path)

#separating forward and reverse fastq filenames 
fnFs <- sort(list.files(path, pattern="_F.fastq$", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="_R.fastq$", full.names = TRUE))

#need to make sure there is a forward and reverse read for each sample or else merge pairs breaks

#extracting sample ids from forward & reverse reads
#note to self: sub replaces the end of the file name with blanks so they actually can match up 
sampleF <- sub("_F.fastq$", "", basename(fnFs))
sampleR <- sub("_R.fastq$", "", basename(fnRs))

#filtering for samples that have a pair 
keepF <- sampleF %in% sampleR
keepR <- sampleR %in% sampleF

fnFs <- fnFs[keepF]
fnRs <- fnRs[keepR]

sampleF <- sampleF[keepF]
sampleR <- sampleR[keepR]

#reorder reverse list to match forward order (mergepairs only words if in order)
fnRs <- fnRs[match(sampleF, sampleR)]
sampleR <- sampleR[match(sampleF, sampleR)]

#checking if it worked
all(sampleF == sampleR)

#calculating errors 
errF <- learnErrors(fnFs, multithread=TRUE)
errR <- learnErrors(fnRs, multithread=TRUE)

#plotting errors 
plotErrors(errF, nominalQ=TRUE)

#turning in dada class objects 
dadaFs <- dada(fnFs, err=errF, multithread=TRUE)
dadaRs <- dada(fnRs, err=errR, multithread=TRUE)

#checking Dada class object 
dadaFs[[1]]

#merge pair reads 
mergers <- mergePairs(dadaFs, fnFs, dadaRs, fnRs, verbose=TRUE, maxMismatch = 1000)
# will get error is there is a sequence without a pair (one read)

#if want to see all merges: 
list(mergers)

#check merge df from first sample 
head(mergers[[1]])