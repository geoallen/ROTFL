#hydrograph_plotter.R
# Geroge H. Allen, Aug 28, 2019

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
inDirPath = paste0(wd, '/in')
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
# Exclude observations that are estimated/provisional/frozen:
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
# x = apply(Master_Code, 2, grep, pattern="p|Ice|E", ignore.case=T)
# 100*sum(sapply(x, length))/(nrow(Master_Code)*ncol(Master_Code))

# results: 
# A = approved → 49.20% of records 
# E, E = estimated → 4.49% 
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

for (i in 1:length(qTabVarName)){
  print(paste("excluding low quality observations from", qTabVarName[i]))
  
  # get Q and code tabs:
  qTab = get(qTabVarNames[i])
  cTab = get(cTabVarNames[i])
  
  # run through each column and set problem observations to NA:
  for (j in 1:ncol(qTab)){
    qTab[grep("p|Ice|E", cTab[,j], ignore.case=T), j] = NA
  }
  assign(qTabVarNames[i], qTab)
}


################################################################################
# Combine missions and put in list of list of tables:
################################################################################
tabList578 = as.list(datatype)
tabSetList578 = as.list(dataset)

for (h in 1:length(datatype)){
  for (i in 1:length(dataset)){
    for (j in 1:length(mission)){
      # read in date tab:
      grepStr = paste(mission[j], datatype[h], dataset[i],  sep=".*")
      tabVarName = grep(grepStr, tabNames, value=T)
      tab = get(tabVarName)
      
      if (j==1){
        tab578 = tab
      }else{
        tab578 = rbind(tab578, tab)
      }
    }
    tabSetList578[[i]] = tab578
  }
  tabList578[[h]] = tabSetList578
}


################################################################################
# Plot hydrographs:
################################################################################

pdfOutPath = paste0(outDirPath, "/hydrographs.pdf")

# set up plot:
pdf(pdfOutPath, 14, 4)


# set up sampling tables:
Dtab_cf = tabList578[[1]][[1]]
Qtab_cf = tabList578[[2]][[1]]
Dtab_ar = tabList578[[1]][[2]]
Qtab_ar = tabList578[[2]][[2]]

gaugeID = names(Master_Value)

# for each gauge:
for (i in 1:ncol(Master_Value)){
  
  Date = as.Date(as.POSIXct(Master_Date[,i], origin="1970-01-01"))
  Q_cms = Master_Value[,i]*0.028316846592
  
  plot(Date, Q_cms, type="l", 
    main=paste("Site:", sub("X", "", gaugeID[i])), 
    lwd=0.4)
    
  # add all returns samples:
  Date_ar = as.Date(as.POSIXct(Dtab_ar[,i], origin="1970-01-01"))
  Q_cms_ar = Qtab_ar[,i]*0.028316846592
  
  points(Date_ar, Q_cms_ar)
  
  # add cloud free samples:
  Date_cf = as.Date(as.POSIXct(Dtab_cf[,i], origin="1970-01-01"))
  Q_cms_cf = Qtab_cf[,i]*0.028316846592
  
  points(Date_cf, Q_cms_cf, pch=3, col=2)
  
  # plot distributions on y-axis:
  
  # coming soon
  
}




dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)

