#quantile_plotter.R
# Geroge H. Allen, Aug 19, 2019

################################################################################
# Description
################################################################################

# reads in all data tables and plots population discharge quantiles 
# against sample discharge quntiles. Outputs a pdf figure.

# runs wilcox (Mann-Whitney) non-parametric test to determine if sample is  
# statistically different than population. 


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
# Exclude observations that are estimated/frozen:
################################################################################
# list of codes: https://help.waterdata.usgs.gov/codes-and-parameters/parameters 

# Get all unique codes in codes table:
uniqCodes = unique(as.vector(unlist(apply(Master_Code, 2, unique))))

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

# exclude Ice, provisional, and estimated codes (set Master_Value cells to NA):
for (i in 1:ncol(Master_Value)){
  Master_Value[grep("p|Ice|E", Master_Code[,i], ignore.case=T), i] = NA
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
# Calculate discharge quantiles for individual missions and datasets
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
    for (k in 1:length(probs)){
      tab_qTab[k,] = as.vector(apply(tab, 2, quantile, probs=probs[k], na.rm=T))
    }
    qTabList[[tabVarInd]] = tab_qTab
  }
}

################################################################################
# plot discharge quantiles
################################################################################
pdfOutPath = paste0(outDirPath, "/quantile_plots.pdf")

# set up plot:
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
  text(0.5, 0.5, gsub("_", " ", tabNames[plotInd[i]]), cex=2, font=2)
  
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
  
    # Wilcox non-parametrix inferential test:
    # p-val>0.01 means that sample is not statistically different than population:
    wilcox = wilcox.test(Qsam_cms, Qpop_cms, "two.sided", exact=F)
    if (wilcox$p.value < 1e-3){
      legend("bottomright", "Wilcox p-value < 0.001")
    }else{
      legend("bottomright", paste("Wilcox p-value =", round(wilcox$p.value, 3)))
    }
    
    # parametric t-test (likely not correct test due to apparent heteroskedasticity):
    # tTest = t.test(Qsam_cms, Qpop_cms)
    # text(max(Qpop_cms, na.rm=T), min(Qsam_cms, na.rm=T), 
    #      paste("t-test p-value =", tTest$p.value), pos=2)
    
  }
  plot.new()
}

dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)



