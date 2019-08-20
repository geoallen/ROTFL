#quantile_plotter.R
# Geroge H. Allen, Aug 19, 2019

################################################################################
# Description
################################################################################

# reads in all data tables and plots population discharge quantiles 
# against sample discharge quntiles. Outputs a pdf figure. 

# specify working directory:
wd = "/Users/allenstandard/research/2019_08_20_ROTFL/git/ROTFL"

################################################################################
# Read in all CSVs
################################################################################
# put all tables from Drive folder into the "in" directory:
inDirPath = paste0(wd, '/in')
pdfOutPath = paste0(wd, "/out/temp.pdf")
CSVpaths_all = list.files(inDirPath, ".csv", recursive=T, full.names=T)

# hard-coded names of CSV tables:
mission = c("Landsat_5", "Landsat_7", "Landsat_8")
master = "Master"
datatype = c("Date", "Value", "Code")
dataset = c("CloudsRemoved", "AllReturns")


# outer(outer(dataset, datatype, paste, sep=".*"), 
#       mission, paste, sep=".*")


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
# Calculate discharge quantiles for Master table
################################################################################
probs = seq(0, 1, 0.1)

Master_Value_qTab = as.data.frame(array(NA, c(length(probs), ncol(Master_Value))))
for (i in 1:length(probs)){
  print(paste0("Calculating values for ", 100*probs[i], "% quantile"))
  Master_Value_qTab[i,] = as.vector(apply(Master_Value, 2, quantile, probs=probs[i], na.rm=T))
}

################################################################################
# Calculate discharge quantiles for missions and datasets
################################################################################
qTabList = as.list(tabNames)
for (i in 1:length(mission)){
  for (j in 1:length(dataset)){
    grepStr = paste(mission[i], datatype[2], dataset[j],  sep=".*")
    tabVarInd = grep(grepStr, tabNames)
    tabVarName = tabNames[tabVarInd]
    
    print(paste0("Calculating percentile values for: ", tabVarName))
    tab = get(tabVarName)
    
    tab_qTab = as.data.frame(array(NA, c(length(probs), ncol(tab))))
    for (l in 1:length(probs)){
      tab_qTab[l,] = as.vector(apply(tab, 2, quantile, probs=probs[l], na.rm=T))
    }
    qTabList[[tabVarInd]] = tab_qTab
  }
}

################################################################################
# plot discharge quantiles
################################################################################
pdf(pdfOutPath, 14, 12)
par(mar=c(4,4,1,1))
layout(matrix(c(1,1,1,1,
              2,3,4,5,
              6,7,8,9,
              10,11,12,13), nrow=4, byrow=T),
       heights = c(1,4,4,4,4))

plotInd = which(lapply(qTabList, length) > 1)

for (i in 1:length(plotInd)){
  sTab = qTabList[[plotInd[i]]]
  m2s_match = match(names(Master_Value_qTab), names(sTab))
  
  plot.new()
  text(0.5, 0.5, sub("_", " ", tabNames[plotInd[i]]), cex=2, font=2)
  
  for (j in 1:length(probs)){
    
    Qpop_cms = as.numeric(Master_Value_qTab[j,])*0.028316846592
    Qsam_cms = as.numeric(sTab[j, m2s_match])*0.028316846592
    
    plot(Qpop_cms, Qsam_cms, 
         main="",
         xlab="Qpopulation (cms)",
         ylab="Qsample (cms)",
         log="xy", asp=1, 
         pch=16, cex=0.8, col=rgb(0,0,0,0.2))
    title(paste0("Quantile: ", probs[j]*100, "%"), line=-1)
    abline(0, 1, lwd=0.5)
  
    # run a 1-way Z-test (or a non parametric equivalent) here
  }
  plot.new()
}

dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)



