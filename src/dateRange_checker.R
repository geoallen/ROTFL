#dateRange_checker.R
# Geroge H. Allen, Aug 28, 2019

################################################################################
# Description
################################################################################

# reads in all data tables and prints starting and ending dates of each sample:

# specify working directory:
wd = "/Users/allenstandard/research/2019_08_19_ROTFL/git/ROTFL"

################################################################################
# Read in all CSVs
################################################################################
# put all tables from Drive folder into the "in" directory:
inDirPath = paste0(wd, '/in')
if (!file.exists(inDirPath)){dir.create(inDirPath)}
CSVpaths_all = list.files(inDirPath, ".csv", recursive=T, full.names=T)

# write out PDFs in "out" directory:
outDirPath = paste0(wd, "/out")
if (!file.exists(outDirPath)){dir.create(outDirPath)}

# hard-coded names of CSV tables:
mission = c("Landsat_5", "Landsat_7", "Landsat_8")
master = c("Master")
datatype = c("Date")
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
# Print date ranges for missions:
################################################################################
# set up date range table:
colNames = c("mission", "start_date", "end_date")
dateRangeTab = as.data.frame(array(NA, c(length(mission)*length(dataset), length(colNames))))
names(dateRangeTab) = colNames

for (i in 1:length(tabNames)){
  # get date table:
  tabVarInd = grep(grepTerms[i], tabNames)
  tabVarName = tabNames[tabVarInd]
  dTab = as.data.frame(get(tabVarName))
  
  firstDate = as.Date(as.POSIXct(min(dTab, na.rm=T), origin="1970-01-01"))
  lastDate = as.Date(as.POSIXct(max(dTab, na.rm=T), origin="1970-01-01"))
  
  dateRangeTab[i,1] = tabVarName
  dateRangeTab[i,2] = as.character(firstDate)
    dateRangeTab[i,3] = as.character(lastDate)
}

print(dateRangeTab)


# The dates in dateRangeTab should match the following dates:
# L5_firstScene = "shortly after 1984_03_01"
# L5_lastsScene = "2013_06_05"
# L7_firstScene = "1999_04_18"
# L7_lastsScene = "near present"
# L8_firstScene = "2013_03_18"
# L8_lastsScene = "near present"
# Master_first = "shortly after 1984_03_01 or later"
# Master_last = "near present"

