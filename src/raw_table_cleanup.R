#raw_table_cleanup.R
# Geroge H. Allen, Sept 06, 2019

################################################################################
# Description
################################################################################

# reads in all data tables and plots sample hydrographs with samples dates:

# specify working directory:
wd = "/Users/allenstandard/research/2019_08_19_ROTFL/git/ROTFL"

################################################################################
# Read in all CSVs
################################################################################
# put all tables from Drive folder into the "in" directory:
inDirPath = paste0(wd, '/in/raw')
if (!file.exists(inDirPath)){dir.create(inDirPath)}
CSVpaths_all = list.files(inDirPath, ".csv", recursive=T, full.names=T)

# write out PDFs in "out" directory:
outDirPath = paste0(wd, "/out")
if (!file.exists(outDirPath)){dir.create(outDirPath)}

# hard-coded names of CSV tables:
mission = c("Landsat_5", "Landsat_7", "Landsat_8")
master = c("Master")
datatype = c("Date", "Value", "Code")
dataset = c("CloudsRemoved", "AllReturns")

grepTerms = c(as.vector(outer(outer(mission, datatype, paste, sep=".*"), 
                              dataset, paste, sep=".*")), 
              paste(master, datatype, sep=".*"))
tabNames = gsub("[.]", "_", gsub("[*]", "", grepTerms))

pathNamesList = lapply(grepTerms, grep, CSVpaths_all, value=T)

# read in csvs and assign them to variables based on file names:
for (i in 1:length(tabNames)){
  print(paste("Reading in", pathNamesList[[i]][1]))
  assign(tabNames[i], read.csv(pathNamesList[[i]][1], header=T))
}


################################################################################
# Exclude gauges with short records:
################################################################################
# set up date range table:
tab = get("Master_Date")

L = as.vector(apply(tab, 2, function(x){length(which(!is.na(x)))}))

# which records have a length less than a given number of years?
shortRecords = names(tab)[which(L/365 <= 5)]

print(paste(length(shortRecords), "of", length(L), "short records removed"))

# 100 * (1-length(shortRecords) / length(L))
# 91% of gauges have recodrs lengths > 5 years
# 87% of gauges have recodrs lengths > 10 years

# set gauges with short records to NA:
for (i in 1:length(tabNames)){
  tab = get(tabNames[i])
  
  nameMatch = match(shortRecords, names(tab))
  tab[, nameMatch] = NA
  
  assign(tabNames[i], tab)
  
}



################################################################################
# Exclude gauge observations that are estimated/provisional/frozen:
################################################################################
# list of codes: https://help.waterdata.usgs.gov/codes-and-parameters/parameters 

# Get all unique codes in codes table:
# uniqCodes = unique(as.vector(unlist(apply(Master_Code, 2, unique))))
# [1] "A"     "A e"   "P"     "P e"   NA      "P Ice" "P Eqp" "P Mnt" "P [4]" "A <"   "A R"  
# [12] "P Rat" "P ***" "P Ssn" "A >"   "P Bkw" "A [4]"

# code description page seems to have recently changed:
# https://help.waterdata.usgs.gov/codes-and-parameters/discharge-measurement-quality-code
# A = approved
# E, E = estimated
# P = provisional
# < = underestimated
# > = overestimated
# 1, 2 = write protected
# : = and

# e, E, P, Ice should be removed?

# count number of occurences for codes of interest:
# x = apply(Master_Code, 2, grep, pattern=" ", ignore.case=T)
# x = apply(Master_Code, 2, grep, pattern="a", ignore.case=T)
# x = apply(Master_Code, 2, grep, pattern="ice", ignore.case=T)
# x = apply(Master_Code, 2, grep, pattern="p", ignore.case=T)
# x = apply(Master_Code, 2, function(x){which(is.na(x))})
# x = apply(Master_Code, 2, grep, pattern="p|Ice|E|>|<", ignore.case=T)
# 100*sum(sapply(x, length))/(nrow(Master_Code)*ncol(Master_Code))

# results: 
# A = approved → 49.20% of records 
# e, E = estimated → 4.49% 
# P = provisional → 0.82%
# < = underestimated
# > = overestimated
# 1, 2 = write protected
# : = and
# Ice = ice → 0.027%
# NA = blank → 49.98% 


# exclude Ice, provisional, and estimated codes (set value cells to NA):
qTabVarNames = tabNames[grep("Value", tabNames)]
cTabVarNames = tabNames[grep("Code", tabNames)]

for (i in 1:length(qTabVarNames)){
  print(paste("excluding low quality observations from", qTabVarNames[i]))
  
  # get Q and code tabs:
  qTab = get(qTabVarNames[i])
  cTab = get(cTabVarNames[i])
  
  # run through each column and set problem observations to NA:
  for (j in 1:ncol(qTab)){
    qTab[grep("p|Ice|E|>|<", cTab[,j], ignore.case=T), j] = NA
  }
  assign(qTabVarNames[i], qTab)
}


################################################################################
# Exclude Landsat 7 records post SLC error (May 31, 2003):
################################################################################

# Landsat 7 mission:
grepStr = mission[2]
tabVarName = grep(grepStr, tabNames, value=T)

for (i in 1:length(dataset)){
  print(paste("removing post SLC error observations in Landsat 7 for", dataset[i]))
  datasetTabVarName = grep(dataset[i], tabVarName, value=T)
  
  # create a False boolean table with all dates in tab after SLC error set to True:
  dateTabVarName = grep(datatype[1], datasetTabVarName, value=T)
  tab = get(dateTabVarName)
  booTab = as.data.frame(array(F, dim=dim(tab)), row.names=names(tab))
  booTab[tab >= 1054422000] = T # May 31, 2003 --> 1054422000 unix timestamp
  
  # for each table, set post SLC error records to NA:
  for (j in 1:length(datasetTabVarName)){
    tab = get(datasetTabVarName[j]) 
    tab[booTab==T] = NA
    
    assign(datasetTabVarName[j], tab)
    
  }
}


################################################################################
# Write out updated data tables:
################################################################################
outCSVpaths = sub(inDirPath, paste0(outDirPath,"/cleaned"), pathNamesList)
# create directories if they don't exist:
parentDirs = unique(dirname(outCSVpaths))
lapply(parentDirs[!file.exists(parentDirs)], dir.create, recursive=T)

# write out CSVs:
for (i in 1:length(tabNames)){
  print(paste("Writing out:", outCSVpaths[i]))
  write.csv(get(tabNames[i]), outCSVpaths[i], row.names=F)
}





