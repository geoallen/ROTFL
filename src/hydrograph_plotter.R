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

par(mar=c(4,4,1,1))
layout(matrix(c(1,2), ncol=2, byrow=T),
       widths = c(1,4))

# for each gauge:
for (i in 1:ncol(Master_Value)){
  
  # master tables:
  Date = as.Date(as.POSIXct(Master_Date[,i], origin="1970-01-01"))
  Q_cms = Master_Value[,i]*0.028316846592
  # all returns samples:
  Date_ar = as.Date(as.POSIXct(Dtab_ar[,i], origin="1970-01-01"))
  Q_cms_ar = Qtab_ar[,i]*0.028316846592
  # cloud free samples:
  Date_cf = as.Date(as.POSIXct(Dtab_cf[,i], origin="1970-01-01"))
  Q_cms_cf = Qtab_cf[,i]*0.028316846592
  
  # plot density distributions on y-axis:
  kern = density(Q_cms, na.rm=T)
  plot(kern$y, kern$x, type="l",
       xlab = "Density",
       ylab = "Discharge (cms)",
       bty="n")
  kern = density(Q_cms_ar, na.rm=T)
  lines(kern$y, kern$x, col="blue")
  kern = density(Q_cms_cf, na.rm=T)
  lines(kern$y, kern$x,bty="n", col="red")
  
  
  plot(Date, Q_cms, type="l", 
    main=paste("Site:", sub("X", "", gaugeID[i])), 
    lwd=0.4)
  points(Date_ar, Q_cms_ar, cex=0.8, col="blue")
  points(Date_cf, Q_cms_cf, pch=3, cex=0.8, col="red")
  
}




dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)

