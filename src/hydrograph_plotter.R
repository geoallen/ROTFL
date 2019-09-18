#hydrograph_plotter.R
# Geroge H. Allen, Aug 28, 2019

################################################################################
# Description
################################################################################

# reads in all data tables and plots sample hydrographs with samples dates:


#install.packages("Matching")
library(Matching)

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
# set up single multipage plot:
# pdfOutPath = paste0(outDirPath, "/hydrographs_full.pdf")
# pdf(pdfOutPath, 10, 3.5)

# alternitively, set up a plot multi single-page plot:
pdfOutDir = paste0(outDirPath, "/hydrographs")
if (!file.exists(pdfOutDir)){ dir.create(pdfOutDir)}
pdfOutPath = paste0(pdfOutDir, "/", sub("X", "", gaugeID), ".pdf")


# set up sampling tables:
Dtab_cf = tabList578[[1]][[1]]
Qtab_cf = tabList578[[2]][[1]]
Dtab_ar = tabList578[[1]][[2]]
Qtab_ar = tabList578[[2]][[2]]

gaugeID = names(Master_Value)



# for each gauge:
for (i in 1:ncol(Master_Value)){ # round(seq(1, ncol(Master_Value), length.out=25))){ #  
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
  
  # # full (raw) master tables:
  # Date_raw_full = as.Date(as.POSIXct(Master_Date_full[,i], origin="1970-01-01"))
  # Q_cms_raw_full = Master_Value_full[,i]*0.028316846592
  #
  # # handle irregular NA values for full (raw) records:
  # naBoo = is.na(Date_raw_full) | is.na(Q_cms_raw_full)
  # Date_full = Date_raw_full[!naBoo]
  # Q_cms_full = Q_cms_raw_full[!naBoo]
  # jumps = rev(which(diff(Date_full) > 1))+1
  # if (length(jumps) > 0){
  #   for (j in 1:length(jumps)){
  #     Date_full = insertRow(Date_full, NA, jumps[j])
  #     Q_cms_full = insertRow(Q_cms_full, NA, jumps[j])
  #   }
  # }
  
 
  
  # open pdf device: 
  pdf(pdfOutPath[i], 14, 4)
  
  
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
  
  
  # plot density distributions on y-axis:
  par(new=T)
  layout(matrix(c(1,2), ncol=2, byrow=T),
         widths = c(2,1))
  
  # par(mar=c(4,4,1,1))
  # kern = density(Q_cms, na.rm=T, bw="nrd0")
  # kern_ar = density(Q_cms_ar, na.rm=T, bw="nrd0")
  # kern_cf = density(Q_cms_cf, na.rm=T, bw="nrd0")
  # plot(kern$y, kern$x, type="l",
  #      ylim = range(Q_cms, na.rm=T),
  #      xlim = range(c(kern$y, kern_ar$y, kern_cf$y)), 
  #      xlab = "Density",
  #      ylab = "",
  #      axes=T,
  #      yaxt="n",
  #      bty="n", lwd=1.4)
  # lines(kern_ar$y, kern_ar$x, col=4, lwd=1.4)
  # lines(kern_cf$y, kern_cf$x, bty="n", col=2, lwd=1.4)
  
  
  par(mar=c(4,4,1,1))
  # kern = density(Q_cms, na.rm=T, bw="nrd0")
  # kern_ar = density(Q_cms_ar, na.rm=T, bw="nrd0")
  # kern_cf = density(Q_cms_cf, na.rm=T, bw="nrd0")
  Q_cms_ePDF = ePDF(Q_cms, Q_cms)
  Q_cms_ar_ePDF = ePDF(Q_cms_ar, Q_cms)
  Q_cms_cf_ePDF = ePDF(Q_cms_cf, Q_cms)
  
  plot(Q_cms_ePDF$y, Q_cms_ePDF$x, type="l", 
       xlab = "Cumulative Density",
       ylab = "",
       axes=T,
       yaxt="n",
       bty="n", lwd=1.4)
  lines(Q_cms_ar_ePDF$y, Q_cms_ar_ePDF$x, col=4, lwd=1.4)
  lines(Q_cms_cf_ePDF$y, Q_cms_cf_ePDF$x, col=2, lwd=1.4)
  
  # plot cleaned hydrograph and sampling time:
  par(new=T)
  layout(matrix(c(2,1), ncol=2, byrow=T),
         widths = c(3,1))
  par(mar=c(4,4,1,0))
  plot(Date, Q_cms, type="l",
       ylim=range(Q_cms, na.rm=T),
       xlab="Date", ylab="",
       bty="n",
       axes=F,
       lwd=0.4, col=1)
  points(Date_ar, Q_cms_ar, cex=0.4, col=rgb(0,0,1,0.7))
  points(Date_cf, Q_cms_cf, pch=3, cex=0.4, col=rgb(1,0,0,0.7))
  Jan1 = as.Date(grep("*01-01$", Date_raw, value=T))
  lab = Jan1[grep("5|0", substr(Jan1, 4, 4))]
  axis(1, Jan1, labels=F, lwd.ticks=0.5)
  axis(1, lab, format(lab, "%Y"), line=F)
  
  # add statistics:
  # ks_ar = Matching::ks.boot(Q_cms, Q_cms_ar, nboots=500)
  # ks_ar_D = round(ks_ar$ks$statistic[[1]], 2)
  # if (ks_ar$ks$p.value < 1e-3){ ks_ar_p =  "< 0.001" }else{ ks_ar_p = paste("=", round(ks_ar$ks$p.value, 3)) }
  # Matching::ks.boot(Q_cms, Q_cms_cf, nboots=500)
  # ks_cf_D = round(ks_cf$ks$statistic[[1]], 2)
  # if (ks_cf$ks$p.value < 1e-3){ ks_cf_p =  "< 0.001" }else{ ks_cf_p = paste("=", round(ks_cf$ks$p.value, 3)) }
  
  ks_ar = suppressWarnings(ks.test(Q_cms, Q_cms_ar)) 
  ks_ar_D = round(ks_ar$statistic[[1]], 2)
  if (ks_ar$p.value < 1e-3){ ks_ar_p =  "< 0.001" }else{ ks_ar_p = paste("=", round(ks_ar$p.value, 3)) }
  ks_cf = suppressWarnings(ks.test(Q_cms, Q_cms_cf)) 
  ks_cf_D = round(ks_cf$statistic[[1]], 2)
  if (ks_cf$p.value < 1e-3){ ks_cf_p =  "< 0.001" }else{ ks_cf_p = paste("=", round(ks_cf$p.value, 3)) }
  
  
  legend("topleft", c("Gauge record", 
                      paste0("All returns: k-s D = ", ks_ar_D, ",  p-val ", ks_ar_p),
                      paste0("Clear-sky returns: k-s D = ", ks_cf_D, ";  p-val ", ks_cf_p)),
         text.col=c(1, 4, 2), cex=0.8, bg="white")
  
  # # plot full (raw) hydrograph:
  # par(new=T)
  # layout(matrix(c(1,2), ncol=2, byrow=T),
  #        widths = c(1,3))
  # par(mar=c(4,4,1,1))
  # plot(Date_full, Q_cms_full, type="l", 
  #      ylim=range(Q_cms_full, na.rm=T),
  #      xlab="Date", ylab="",
  #      bty="n",
  #      axes=F, 
  #      lwd=0.4, col="gray")
  # 
  # # plot cleaned hydrograph and sampling time:
  # par(new=T)
  # lines(Date, Q_cms,  
  #      xlab="", ylab="",
  #      bty="n",
  #      lwd=0.4, col=rgb(0,0,0,1))
  # points(Date_ar, Q_cms_ar, cex=0.4, col=rgb(0,0,1,0.7))
  # points(Date_cf, Q_cms_cf, pch=3, cex=0.4, col=rgb(1,0,0,0.7))
  # Jan1 = as.Date(grep("*01-01$", Date_raw, value=T))
  # lab = Jan1[grep("5|0", substr(Jan1, 4, 4))]
  # axis(1, Jan1, labels=F, lwd.ticks=0.5)
  # axis(1, lab, format(lab, "%Y"), line=F)
  # 
  # legend("topright", c("Full gauge record", "Approved gauge record", "All returns", "Clear sky returns"), 
  #        text.col=c("gray", 1, 4, 2), cex=0.8, bg="white")
  
  
  # close pdf device:
  dev.off()
  
}


# dev.off()
# cmd = paste('open', pdfOutPath)
# system(cmd)




















# ################################################################################
# # Plot hydrographs with a subset:
# ################################################################################
# 
# pdfOutPath = paste0(outDirPath, "/hydrographs_1yr.pdf")
# 
# # set up plot:
# pdf(pdfOutPath, 14, 8)
# 
# 
# # set up sampling tables:
# Dtab_cf = tabList578[[1]][[1]]
# Qtab_cf = tabList578[[2]][[1]]
# Dtab_ar = tabList578[[1]][[2]]
# Qtab_ar = tabList578[[2]][[2]]
# 
# gaugeID = names(Master_Value)
# 
# 
# # for each gauge:
# for (i in 1:20){ #ncol(Master_Value)){
#   #print(i)
#   # master tables:
#   Date = as.Date(as.POSIXct(Master_Date[,i], origin="1970-01-01"))
#   Q_cms = Master_Value[,i]*0.028316846592
#   # all returns samples:
#   Date_ar = as.Date(as.POSIXct(Dtab_ar[,i], origin="1970-01-01"))
#   Q_cms_ar = Qtab_ar[,i]*0.028316846592
#   # cloud free samples:
#   Date_cf = as.Date(as.POSIXct(Dtab_cf[,i], origin="1970-01-01"))
#   Q_cms_cf = Qtab_cf[,i]*0.028316846592
#   
#   if (length(which(!is.na(Date))) < 365*5){next}
#   
#   
#   # add horizontal lines to plot:
#   layout(matrix(c(1,1,2,2), ncol=2, byrow=T),
#          widths = c(1,3,2,2))
#   par(mar=c(4,4,1,1))
#   plot(Date, Q_cms, type="n", 
#        ylim=range(Q_cms, na.rm=T), 
#        bty="n", axes=F, xlab="", ylab="",
#        main=paste("Site:", sub("X", "", gaugeID[i])))
#   abline(h=axis(2), lwd=0.2)
#   box()
#   
#   
#   # plot density distributions on y-axis:
#   par(new=T)
#   layout(matrix(c(3,2,1,1), ncol=2, byrow=T),
#          widths = c(1,3,2,2))
#   
#   par(mar=c(4,4,1,0))
#   kern = density(Q_cms, na.rm=T, bw="nrd0")
#   kern_ar = density(Q_cms_ar, na.rm=T, bw="nrd0")
#   kern_cf = density(Q_cms_cf, na.rm=T, bw="nrd0")
#   plot(kern$y, kern$x, type="l",
#        ylim = range(Q_cms, na.rm=T),
#        xlim = range(c(kern$y, kern_ar$y, kern_cf$y)), 
#        xlab = "Density",
#        ylab = "Discharge (cms)",
#        axes=T,
#        yaxt="n",
#        bty="n")
#   lines(kern_ar$y, kern_ar$x, col="blue")
#   lines(kern_cf$y, kern_cf$x, bty="n", col="red")
#   
#   
#   
#   # plot hydrograph and sampling time:
#   par(new=T)
#   layout(matrix(c(3,4,1,2), ncol=2, byrow=T),
#          widths = c(1,3,2,2))
#   par(mar=c(4,0,1,1))
#   plot(Date, Q_cms, type="l", 
#        ylim=range(Q_cms, na.rm=T),
#        xlab="Date", ylab="",
#        bty="n",
#        axes=F, #yaxt="n",
#        lwd=0.4)
#   points(Date_ar, Q_cms_ar, cex=0.4, col="blue")
#   points(Date_cf, Q_cms_cf, pch=3, cex=0.4, col="red")
#   Jan1 = as.Date(grep("*01-01$", Date, value=T))
#   lab = Jan1[grep("5|0", substr(Jan1, 4, 4))]
#   axis(1, Jan1, labels=F, lwd.ticks=0.5)
#   axis(1, lab, format(lab, "%Y"), line=F)
#   
#   legend("topright", c("Full gauge record", "All returns", "Clear sky returns"), 
#          text.col=c(1, 4, 2), cex=0.8, bg="white")
#   
#   
#   # add a subset plot: 
#   subInd = grep(substr(Date[round(length(which(!is.na(Date)))/2)], 1, 4), Date)
#   
#   Date_sub = Date[subInd]
#   Q_cms_sub = Q_cms[subInd]
#   Date_ar_sub = Date_ar[which(Date_ar>min(Date_sub) & Date_ar<max(Date_sub))]
#   Q_cms_ar_sub = Q_cms_ar[which(Date_ar>min(Date_sub) & Date_ar<max(Date_sub))]
#   Date_cf_sub = Date_cf[which(Date_cf>min(Date_sub) & Date_cf<max(Date_sub))]
#   Q_cms_cf_sub = Q_cms_cf[which(Date_cf>min(Date_sub) & Date_cf<max(Date_sub))]
#     
#   par(new=T)
#   layout(matrix(c(1,2,3,4,5,5,5,5), ncol=4, byrow=T),
#          widths = c(1,1,1,1,1,1,1,1))
#   par(mar=c(4,4,1,1))
#   plot(Date_sub, Q_cms_sub, type="l", 
#        ylim=range(Q_cms, na.rm=T),
#        xlab="Date", ylab="",
#        xaxt="n",
#        lwd=0.4)
#   points(Date_ar_sub, Q_cms_ar_sub, cex=0.8, col="blue")
#   points(Date_cf_sub, Q_cms_cf_sub, pch=3, cex=0.8, col="red")
#   fdotm = as.Date(grep("*01$", Date_sub, value=T))
#   axis(1, fdotm, format(fdotm, "%b %Y"), line=F)
#   
#   legend("topright", c("Full gauge record", "All returns", "Clear sky returns"), 
#          text.col=c(1, 4, 2), cex=0.8, bg="white")
# }
# 
# 
# dev.off()
# cmd = paste('open', pdfOutPath)
# system(cmd)





