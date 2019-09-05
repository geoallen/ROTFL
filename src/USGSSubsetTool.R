####create subset discharge records using dates of cloud-free landsat imagery###############
############################################################################################
# Author: Joel Holliman

library(dataRetrieval)
#load tables of dates 
GaugeNumbers <- read.csv(file="c:L8CloudRemoved.csv", sep=",",header=TRUE)

#load master tables
DischargeValues = read.csv(file="c:DischargeValues.csv", sep=",",header=TRUE)
DischargeDates = read.csv(file="c:DischargeDates.csv", sep=",",header=TRUE)
DischargeCodes = read.csv(file="c:DischargeCodes.csv", sep=",",header=TRUE)

#create data frames to store subsets of discharge values, dates of measurments, and quality codes
SatQValues = data.frame(matrix(ncol=1134))
SatQDates = data.frame(matrix(ncol=1134))
SatQCodes = data.frame(matrix(ncol=1134))
ColumnNames = colnames(DischargeValues)


colnames(SatQValues)=(ColumnNames)
colnames(SatQDates)=(ColumnNames)
colnames(SatQCodes)=(ColumnNames)


#for each gauge
for(i in 1:1134){
  #########get dates in a more usable format
  LSDates = GaugeNumbers[i,]
  LSDates1 = as.character((LSDates[1,2]))
  LSDates2 = data.frame(unlist(strsplit(LSDates1,',')))
  imgdatesp = data.frame(as.numeric((as.POSIXct(LSDates2[,1],"GMT" ,"%Y-%m-%d"))))
  imgdates = unique(na.omit(imgdatesp))#final form of dates
  ###########################################
  
  #output vectors
  SatValuesER = vector()
  SatDatesER = vector()
  SatCodesER = vector()
  
  #intersection of USGS dates and satellite dates
  intersection = which(c(imgdates[,1]) %in% c(DischargeDates[,i]))
  
  #function for determining position of a discharge date in original USGS matrix
  FOMI = function(x){
    unixDate = imgdates[x,1]
    MatrixIndex = which(DischargeDates[,i]==unixDate)
    return (MatrixIndex)
  }
  
  if (length(intersection)>0)
  {
    #for every date that is shared between full record and landsat image dates
    for (k in 1:length(intersection))
    {
      #find discharge value, date and code for every img date 
      MatrixIndex = FOMI(intersection[k])
      DischargeValue = DischargeValues[MatrixIndex,i]
      DischargeDate = DischargeDates[MatrixIndex,i]
      DischargeCode = as.character(DischargeCodes[MatrixIndex,i])
      
      #store value
       SatValuesER = append(SatValuesER,DischargeValue)
       SatDatesER = append(SatDatesER,DischargeDate)
       SatCodesER = append(SatCodesER,DischargeCode)
    
    }
    
    #use these to create USGS record that has a start,end that is same as satellite record
    firstDate = FOMI(intersection[1])
    endDate = FOMI(intersection[length(intersection)])
    
    #put gauge specific vectors into their column in the data frame
    SatQValues[1:length(SatValuesER),i]=SatValuesER
    SatQDates[1:length(SatDatesER),i]=SatDatesER
    SatQCodes[1:length(SatCodesER),i]=SatCodesER
    
    #gauge specific version of the "full record" starting on first sat img date and ending on last sat img date
    USGSrecord = c(DischargeValues[(firstDate:endDate),i])
  } 
} 

  

#write data frames to CSV
write.csv(SatQDates,"DischargeDates_Landsat8_CloudsRemoved.csv",row.names = FALSE)
write.csv(SatQValues,"DischargeValues_Landsat8_CloudsRemoved.csv",row.names = FALSE)
write.csv(SatQCodes,"DischargeCodes_Landsat8_CloudsRemoved.csv",row.names = FALSE)
