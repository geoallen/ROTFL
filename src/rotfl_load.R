
library(tidyverse)
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(modeest)
library(sf)
library(mapview)

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
# mission join the data individual and do NOT drop duplicates
ls_all <- bind_rows(ls5, ls7, ls8) %>%
  distinct(site, date, flow_sample, .keep_all = T)

# dups <- ls_all %>%
#   drop_na() %>%
#   group_by(site, date,flow_sample) %>%
#   mutate(n = n()) %>%
#   filter(n >1) %>%
#   arrange(site, date, flow_sample)


### join landsat "sampled" flow to master "population" flow
ms_join <- ms_data %>%
  rename(flow_pop = flow) %>%
  left_join(ls_all, by=c("site", "date")) %>%
# remove data with ice IF is estimated Q/ice AND winter month 
  mutate(month = month(date),
         ice_flag = ifelse(grepl("e", code) ==T, "ice", NA),
         winter = ifelse(month %in% c(11,12,1,2), "winter", NA),
         ice_winter = ifelse(ice_flag =="ice" & winter =="winter", "ice", NA)) %>%
  filter(is.na(ice_winter),
         flow_pop >=0)

# But we can use this to take quick looks at the distributions of Q for a few sites.
# change the index numbers to pan around other sites
ms_join %>%
 # dplyr::filter(site %in% unique(site)[100:120]) %>%
  dplyr::filter(site=="7139000") %>%
  ggplot() +
  geom_histogram(aes(flow_pop), fill="red", alpha=0.5) +
  geom_histogram(aes(flow_sample), fill="blue", alpha=0.5) +
#  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~site, scales="free")

# make look up table of population flow percentiles by site to later join
# to find what population percentiles corresponed with min/max sample Q
percentiles_pop <- ms_join %>%
  dplyr::select(site, flow_pop) %>%
  arrange(site, flow_pop) %>%
  group_by(site) %>%
  mutate(prob_pop = 1-cume_dist(flow_pop)) %>%
  distinct(site, flow_pop, prob_pop, .keep_all = T)
  

# Distributions look good. I think we can extract mode, max, min per gage.
# find mode, min, max for population and sample per site
join_sum <- ms_join %>%
  dplyr::select(site, flow_sample, flow_pop) %>%
  arrange(site, flow_pop) %>%
  group_by(site) %>%
  # remove sites that have no landsat samples, or no gage flow data
  mutate(na_sample = sum(is.na(flow_sample)),
         na_pop = sum(is.na(flow_pop)),
         n = n(),
         ndays_pop = sum(!is.na(flow_pop))) %>%
  filter(n > na_sample & n > na_pop) %>%
  ungroup() %>%
  # calculate modal, max, min Q for sample and population
  group_by(site, ndays_pop) %>%
  summarise_at(vars(flow_sample, flow_pop), funs(mode=modeest::mlv, min=min, max=max), na.rm=T) %>%
  mutate(mode_ratio = flow_sample_mode / flow_pop_mode) %>%
  ungroup() %>%
  # join in what "poplution" flow percentile corresponds to sample max/min 
  inner_join(., percentiles_pop, by=c("site", "flow_sample_max"="flow_pop")) %>%
  rename(prob_sample_max = prob_pop ) %>% 
  inner_join(., percentiles_pop, by=c("site", "flow_sample_min"="flow_pop")) %>%
  rename(prob_sample_min = prob_pop) %>% 
  mutate(percentile_range_sample = (prob_sample_min - prob_sample_max) *100)
  
# doesn't work well for arid rivers that do not have lognormal flow distribution
# these rivers have more of an exponential or some other EVD

join_sum_site <- join_sum %>%
  left_join(site_info, by = "site") %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs= 4326) %>%
  # filter to gages that have > 5 years of Q data
  filter(ndays_pop > 5*365)
  
# make some maps of how well landsat captures modal and range of Q per gage 
mapview(join_sum_site, zcol='percentile_range_sample', legend=T )
mapview(join_sum_site, zcol='mode_ratio', legend=T )


# >90% of gages capture 97% perecntiles of flow
ggplot(join_sum_site) +
  geom_histogram(aes(percentile_range_sample)) +
  scale_y_log10()

# most gages capture close to modal Q. Landsat slightly underestimates on average
ggplot(join_sum) +
  geom_density(aes(mode_ratio), fill="grey") +
  xlim(0, 3.5)+
  theme_bw() +
  geom_vline(xintercept=1, col="red")

ggplot(join_sum_site) +
  geom_point(aes(flow_pop_mode, flow_sample_mode)) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(intercept=0, slope=1, col="red")

# does the length of the Q record (population) impact how well? Not really
ggplot(join_sum_site) +
  geom_point(aes(ndays_pop, mode_ratio)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_hline(yintercept=1, col="red")

# does watershed area affect how well landsat represents modal Q?
# Yes, there is a convergence to better representation as Area increases
ggplot(join_sum_site) +
  geom_point(aes(drain_area_va, mode_ratio)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_hline(yintercept=1, col="red")

# ms_join %>%
#   # dplyr::filter(site %in% unique(site)[1:20]) %>%
#   dplyr::filter(site=="7139000") %>%
#   ggplot(aes(date, flow_pop)) +
#   geom_line() +
#   scale_y_log10()
#  






