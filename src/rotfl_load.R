
library(tidyverse)
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)

### load data
wd <- "D:/GoogleDrive/ROTFL"

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

site_info <- read_csv(paste(wd, "/SiteAttributes.csv", sep="")) %>%
  mutate(site = as.character(site_no)) %>%
  dplyr::select(-site_no)


### function to convert wide to long, and if bind =T, join site data
wide2long <- function(date, value, code, site, bind=F) {

date_long <- date %>%
  gather(key = "site", value = "date") 
  
value_long <- value %>%
  gather(key = "site", value = "flow") %>%
  dplyr::select(-site)

code_long <- code %>%
  gather(key="site", value = "code") %>%
  dplyr::select(-site)

if(bind ==T) {
out <- cbind(date_long, value_long) %>%
  cbind(code_long) %>%
  arrange(site, date) %>%
  mutate(date = anydate(date)) %>%
  separate(site,into=c("x", "site"), sep="X", remove=T, convert=T ) %>%
  mutate(site = as.character(site)) %>%
  dplyr::select(-x) %>%
  left_join(site, b="site")

} else{
  out <- cbind(date_long, value_long) %>%
    cbind(code_long) %>%
    arrange(site, date) %>%
    mutate(date = anydate(date)) %>%
    separate(site,into=c("x", "site"), sep="X", remove=T, convert=T ) %>%
    mutate(site = as.character(site)) %>%
    dplyr::select(-x, -code)
  }

#rm(date, value, code)
return(out)
}

# make master data long
ms_data <- wide2long(date=Master_Date, value=Master_Value, code=Master_Code,
                     site=site_info, bind=T)

# make landsat5 long for clouds removed data
ls5 <- wide2long(date=Landsat_5_Date_CloudsRemoved, value=Landsat_5_Value_CloudsRemoved, 
                      code=Landsat_5_Code_CloudsRemoved, bind=F) %>%
  drop_na() %>%
  mutate(sat = "ls_5") %>%
  rename(flow_sample = flow)
  
# make landsat7 long
ls7 <- wide2long(date=Landsat_7_Date_CloudsRemoved, value=Landsat_7_Value_CloudsRemoved, 
                      code=Landsat_7_Code_CloudsRemoved, bind=F) %>%
  drop_na()  %>%
  mutate(sat = "ls_7") %>%
  rename(flow_sample = flow)

# make landsat8 long
ls8 <- wide2long(date=Landsat_8_Date_CloudsRemoved, value=Landsat_8_Value_CloudsRemoved, 
                      code=Landsat_8_Code_CloudsRemoved, bind=F) %>%
  drop_na() %>%
  mutate(sat = "ls_8") %>%
  rename(flow_sample = flow)

# row bind landsat missions
# there are ~ 34000 duplicates where 5,7 ot 7,8 get sample from same site/date
# I am guessing this is edge of scenes? Lets drop those duplicatesd
# so we can do the stats for all landsat data combined. It doesn't matter which
# mission sampled if its the same site/date, but IF want to do analysis per 
# mission join the data individuall and do NOT drop duplicates
ls_all <- bind_rows(ls5, ls7, ls8) %>%
  distinct(site, date, flow_sample, .keep_all = T)

# dups <- ls_all %>%
#   drop_na() %>%
#   group_by(site, date,flow_sample) %>%
#   mutate(n = n()) %>%
#   filter(n >1) %>%
#   arrange(site, date, flow_sample)

# dups2 <- ms_data %>%
#   drop_na() %>%
#   group_by(site, date,flow) %>%
#   mutate(n = n()) %>%
#   filter(n >1) %>%
#   arrange(site, date, flow)

### join landsat "sampled" flow to master "population" flow
ms_data_join <- ms_data %>%
  rename(flow_pop = flow) %>%
  left_join(ls_all, by=c("site", "date")) %>%
# remove data likely with ice by month and "estimated" Q code
  mutate(month = month(date),
         ice_flag = ifelse(grepl("e", code) ==T, "ice", NA)) %>%
  filter(!month %in% c(11, 12, 1, 2) & is.na(ice_flag))

# But we can use this to take quick looks at the distributions of Q for a few sites
ms_data_join %>%
  dplyr::filter(site %in% unique(site)[1:9]) %>%
  ggplot() +
  geom_histogram(aes(flow_pop), fill="red", alpha=0.5) +
  geom_histogram(aes(flow_sample), fill="blue", alpha=0.5) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~site, scales="free")


unique(ms_data$code)

ls7_data %>%
  drop_na() %>%
  dplyr::filter(site %in% unique(site)[6:10]) %>%
  ggplot(aes(x=date, y=flow)) +
  facet_wrap(~site, scales="free")

# KS test


# find mode, min, max for population and sample per site


# plot KS test by Watershed area




