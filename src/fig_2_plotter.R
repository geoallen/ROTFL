#hydrograph_plotter.R
# Geroge H. Allen, Aug 28, 2019

################################################################################
# Description
################################################################################

# reads in all data tables and plots sample hydrographs with samples dates:


#install.packages("TOSTER")
library(Matching)
#library(TOSTER)

# specify working directory:
wd = "/Users/allenstandard/research/2019_08_19_ROTFL/git/ROTFL"



################################################################################
# Functons
################################################################################
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,length(existingDF)+1)] = existingDF[seq(r,length(existingDF))]
  existingDF[r] = newrow
  return(existingDF)
}

ePDF <- function(data, full_data){
  x = c(min(full_data, na.rm=T), sort(data), max(full_data, na.rm=T))
  
  n = length(which(!is.na(data)))
  y = c(0, seq(1, n, length.out=n)/n, 1)
  return(list(x=x, y=y))
}

################################################################################
# Read in all CSVs
################################################################################

# put all tables from Drive folder into the "in" directory:
inDirPath = paste0(wd, '/in/cleaned')
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


####
# # read in full (raw) Master CSVs for hydrograph plot:
# print("Reading in raw data CSVs...")
# Master_Date_full = read.csv(paste0(wd, '/in/raw/Master_Tables/DischargeDates.csv'), header=T)
# Master_Value_full = read.csv(paste0(wd, '/in/raw/Master_Tables/DischargeValues.csv'), header=T)


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


# sort tables:
dateOrd = apply(Master_Date, 2, order)
for (i in 1:ncol(Master_Date)){
  Master_Date[,i] = Master_Date[dateOrd[,i], i]
  Master_Value[,i] = Master_Value[dateOrd[,i], i]
  Master_Code[,i] = Master_Code[dateOrd[,i], i]
}


################################################################################
# Plot hydrographs:
################################################################################


# set up sampling tables:
Dtab_cf = tabList578[[1]][[1]]
Qtab_cf = tabList578[[2]][[1]]
Dtab_ar = tabList578[[1]][[2]]
Qtab_ar = tabList578[[2]][[2]]
gaugeID = names(Master_Value)

# set up single multipage plot:
pdfOutPath = paste0(outDirPath, "/fig_2.pdf")
pdf(pdfOutPath, 10, 3.5)

##### Figure 2 - Hydrographs:
for (i in c(553,1098)){   # 1:ncol(Master_Value)){ # round(seq(1, ncol(Master_Value), length.out=500))){ #  
  # print(i)
  
  # cleaned master tables:
  Date_raw = as.Date(as.POSIXct(Master_Date[,i], origin="1970-01-01"))
  Q_cms_raw = Master_Value[,i]*0.028316846592
  if (length(which(!is.na(Date_raw))) < 365*5){next}
  
  # all returns samples:
  Date_ar = as.Date(as.POSIXct(Dtab_ar[,i], origin="1970-01-01"))
  Q_cms_ar = Qtab_ar[,i]*0.028316846592
  # cloud free samples:
  Date_cf = as.Date(as.POSIXct(Dtab_cf[,i], origin="1970-01-01"))
  Q_cms_cf = Qtab_cf[,i]*0.028316846592


  # handle irregular NA values for clean records:
  naBoo = is.na(Date_raw) | is.na(Q_cms_raw)
  Date = Date_raw[!naBoo]
  Q_cms = Q_cms_raw[!naBoo]
  jumps = rev(which(diff(Date) > 1))+1
  if (length(jumps) > 0){
    for (j in 1:length(jumps)){
      Date = insertRow(Date, NA, jumps[j])
      Q_cms = insertRow(Q_cms, NA, jumps[j])
    }
  }
  
  
  # add horizontal lines to plot:
  layout(matrix(c(1,1), ncol=2, byrow=T),
         widths = c(3,1))
  par(mar=c(4,4,1,1))
  plot(Date, Q_cms, type="n", 
       ylim=range(Q_cms, na.rm=T), 
       axes=F, 
       xlab="", ylab="Discharge (cms)", las=2,
       main=paste("Site:", sub("X", "", gaugeID[i])))
  abline(h=axis(2), lwd=0.2, col=rgb(0,0,0,0.7))
  box()
  
  
  par(new=T)
  layout(matrix(c(1,2), ncol=2, byrow=T),
         widths = c(2,1))

  
  ####
  # plot CDFs:
  par(mar=c(4,4,1,1))
  Q_cms_ePDF = ePDF(Q_cms, Q_cms)
  # Q_cms_ar_ePDF = ePDF(Q_cms_ar, Q_cms)
  Q_cms_cf_ePDF = ePDF(Q_cms_cf, Q_cms)

  plot(Q_cms_ePDF$y, Q_cms_ePDF$x, type="l",
       xlab = "CDF",
       ylab = "",
       axes=T,
       yaxt="n",
       bty="n", lwd=1.4)
  lines(Q_cms_ar_ePDF$y, Q_cms_ar_ePDF$x, col="dark gray", lwd=1.4)
  lines(Q_cms_cf_ePDF$y, Q_cms_cf_ePDF$x, col=4, lwd=1.4)
  
  legend("topleft", c("Gauge record",
                      paste0("Clear-sky observations"),
                      paste0("Cloudy observations")),
         col=c(1, 4, "dark gray"),
         text.col=c(1, 4, "dark gray"), cex=0.8, bg="white")
  
  
  ####
  # plot a 5-year subset of hydrograph:
  subInd = c(grep("1995", Date)[1]:grep("2000", Date)[1])
  subInd_raw = c(grep("1995", Date_raw)[1]:grep("2000", Date_raw)[1])
  
  par(new=T)
  layout(matrix(c(2,1), ncol=2, byrow=T),
         widths = c(3,1))
  par(mar=c(4,4,1,0))
  plot(Date_raw[subInd_raw], Q_cms_raw[subInd_raw], type="n",
       ylim=range(Q_cms, na.rm=T),
       xlab="Date", ylab="",
       bty="n",
       axes=F)
  
  Date_sub = Date[subInd]
  Q_cms_sub = Q_cms[subInd]
  Date_ar_sub = Date_ar[which(Date_ar>min(Date_sub, na.rm=T) & Date_ar<max(Date_sub, na.rm=T))]
  Q_cms_ar_sub = Q_cms_ar[which(Date_ar>min(Date_sub, na.rm=T) & Date_ar<max(Date_sub, na.rm=T))]
  Date_cf_sub = Date_cf[which(Date_cf>min(Date_sub, na.rm=T) & Date_cf<max(Date_sub, na.rm=T))]
  Q_cms_cf_sub = Q_cms_cf[which(Date_cf>min(Date_sub, na.rm=T) & Date_cf<max(Date_sub, na.rm=T))]

  lines(Date_sub, Q_cms_sub, lwd=1.4, col=1)
  
  points(Date_ar_sub, Q_cms_ar_sub, cex=0.8, col=rgb(0,0,1,1))
  points(Date_cf_sub, Q_cms_cf_sub, pch=16, cex=0.8, col=rgb(1,0,0,1))
  
  Jan1 = as.Date(paste0(c(1995:2000), "-01-01"))
  #axis(1, Jan1, labels=F, lwd.ticks=0.5)
  axis(1, Jan1, format(Jan1, "%Y"), line=F)
  
  # add rigorous statistics:
  # ks_ar = Matching::ks.boot(Q_cms, Q_cms_ar, nboots=500)$ks
  ks_cf = Matching::ks.boot(Q_cms, Q_cms_cf, nboots=500)$ks
  
 
  
}


dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)










