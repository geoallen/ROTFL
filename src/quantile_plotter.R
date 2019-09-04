#quantile_plotter.R
# Geroge H. Allen, Aug 19, 2019

################################################################################
# Description
################################################################################

# reads in all data tables and plots population discharge quantiles 
# against sample discharge quntiles. Outputs a pdf figure.

# runs wilcox (Mann-Whitney) non-parametric test to determine if sample is  
# statistically different than population. 

install.packages("zyp")
library(zyp)

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
qTabListIndiv = as.list(tabNames)
grepStr = datatype[2]
tabVarName_raw = grep(grepStr, tabNames, value=T)
tabVarName = grep("Master", tabVarName_raw, invert=T, value=T)

for (i in 1:length(tabVarName)){
  print(paste0("Calculating percentile values for: ", tabVarName[i]))
  tab = get(tabVarName[i])
  
  tab_qTab = as.data.frame(array(NA, c(length(probs), ncol(tab))))
  for (k in 1:length(probs)){
    tab_qTab[k,] = as.vector(apply(tab, 2, quantile, probs=probs[k], na.rm=T))
  }
  qTabListIndiv[[which(tabVarName[i]==tabNames)]] = tab_qTab
}


################################################################################
# Calculate discharge quantiles for Landsat 5, 7 and 8 combined:
################################################################################

qTabList578 = as.list(dataset)

for (i in 1:length(dataset)){
  grepStr = paste0(datatype[2], ".*", dataset[i])
  tabVarName = grep(grepStr, tabNames, value=T)
  
  print(paste0("Calculating percentile values for: ", dataset[i]))
  tab = rbind(get(tabVarName[1]), get(tabVarName[2]), get(tabVarName[3]))
  
  tab_qTab = as.data.frame(array(NA, c(length(probs), ncol(tab))))
  for (k in 1:length(probs)){
    tab_qTab[k,] = as.vector(apply(tab, 2, quantile, probs=probs[k], na.rm=T))
  }
  qTabList578[[i]] = tab_qTab
}

# combine lists: 
qTabList = c(qTabListIndiv, qTabList578)


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

# remove zero (no flow) or negative (reverse flow) values from master qTab:
Master_Value_qTab[Master_Value_qTab<=0] = NA
xRange = range(Master_Value_qTab, na.rm=T)

# plot computed samples: 
plotInd = which(lapply(qTabList, length) > 1)
for (i in 1:length(plotInd)){
  sTab = qTabList[[plotInd[i]]]
  # remove zero (no flow) or negative (reverse flow) values from qTab:
  sTab[sTab<=0] = NA
  
  m2s_match = match(names(Master_Value_qTab), names(sTab))
  
  plot.new()
  text(0.5, 0.5, gsub("_", " ", c(tabNames, dataset)[plotInd[i]]), cex=2, font=2)
  
  for (j in 1:length(probs)){
    
    Qpop_cms = as.numeric(Master_Value_qTab[j,])*0.028316846592
    Qsam_cms = as.numeric(sTab[j, m2s_match])*0.028316846592
    
    plot(Qpop_cms, Qsam_cms, 
         main="",
         xlab="Qpopulation (cms)",
         ylab="Qsample (cms)",
         log="xy", 
         xlim=xRange, ylim=xRange,
         pch=16, cex=0.8, col=rgb(0,0,0,0.2))
    title(paste0("Quantile: ", probs[j]*100, "%"), line=-1)
    abline(0, 1, lwd=0.5)
  
    # Equivalence test: https://en.wikipedia.org/wiki/Equivalence_test
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3019319/#CR2
    # https://link.springer.com/content/pdf/10.1007%2FBF01068419.pdf
    
    # the equivalence margin, denoted by δ, defines a range of values for which the efficacies 
    # are “close enough” to be considered equivalent.
    # α significance level if a (1–2α) × 100% 
    
    # 'Two One-Sided Tests’ (TOST) procedure:
    # parametric t-test is likely not correct test due to apparent heteroskedasticity:
    
    
    # gt_pVal = t.test(Qsam_cms, Qpop_cms, alternative="greater")$p.value
    # lt_pVal = t.test(Qsam_cms, Qpop_cms, alternative="less")$p.value    
    
    # Noparametric two-one-sided wilcox rank sum test procedure:
    # gt_pVal = wilcox.test(Qsam_cms, Qpop_cms, alternative="greater", exact=F)$p.value
    # lt_pVal = wilcox.test(Qsam_cms, Qpop_cms, alternative="less", exact=F)$p.value
    
    # add a regression line: 
    #reg = lm( log(Qsam_cms) ~ log(Qpop_cms) )  # least squares in log space
    
    
    # Thiel-Sen nonparametric median estimator:
    x = log(Qpop_cms[!(is.na(Qpop_cms) | is.na(Qsam_cms))])
    y = log(Qsam_cms[!(is.na(Qpop_cms) | is.na(Qsam_cms))])
    reg = zyp.sen(y~x)
    # add best fit line:
    xSeq = seq(xRange[1], xRange[2], length.out=100)
    ySeq = exp(reg[[1]][[1]]) * xSeq^reg[[1]][[2]]
    lines(xSeq, ySeq, col=2)
    
    rRMSE = sqrt(sum(((Qsam_cms - Qpop_cms)/Qpop_cms)^2, na.rm=T)/length(Qpop_cms))
    rBIAS = sum((Qsam_cms - Qpop_cms)/Qpop_cms, na.rm=T)/length(Qpop_cms)
    
    # KS test:
    ks = suppressWarnings(ks.test(Qsam_cms, Qpop_cms, "two.sided"))
    
    # # Wilcox non-parametrix inferential test:
    # # p-val>0.01 means that sample is not statistically different than population:
    wilcox = wilcox.test(Qsam_cms, Qpop_cms, "two.sided", exact=F)
    # if (wilcox$p.value < 1e-3){
    #   legend("bottomright", "Wilcox p-value < 0.001")
    # }else{
    #   legend("bottomright", paste("Wilcox p-value =", round(wilcox$p.value, 3)))
    # }
    
    # tTest = t.test(Qsam_cms, Qpop_cms)
    # text(max(Qpop_cms, na.rm=T), min(Qsam_cms, na.rm=T), 
    #      paste("t-test p-value =", tTest$p.value), pos=2)
    
    legend("bottomright", 
           paste("y =",  "e^", round(reg[[1]][[1]], 2), "* x^", round(reg[[1]][[2]], 2), 
                 "\n rRMSE:",  round(rRMSE, 2),
                 "\n rBIAS:",  round(rBIAS, 2),
                 "\n K-S D:",  round(ks[[1]][[1]], 2),
                 "\n K-S p:",  round(ks[[2]][[1]], 4),
                 "\n Wilcox p:", round(wilcox$p.value, 4)),
           bty="n", adj=c(0,-0.25))

    if (ks[[2]][[1]] < 0.05 ){ box(col=2, lwd=2) }
    
  }
  
  plot.new()
  
}

dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)





