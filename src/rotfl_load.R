
library(tidyverse)
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(modeest)
library(sf)
library(mapview)
library(feather)
library(equivalence)
library(ggthemes)
library(stats)
library(MASS)
library(broom)
library(mapview)
library(tidyr)
library(dataRetrieval)
library(htmlwidgets)
library(ggthemes)
library(leafpop)
library(leaflet)
#devtools::install_github("GRousselet/rogme")
library(rogme)
## Equivalence testing
library(equivalence)
library(Matching)
library(twosamples)
 
################################################################

#ls <- read_feather("out/ls_data_long.feather")
#hist(landsat_all %>% group_by(id) %>% summarise(n = sum(!is.na(q))) %>%pull(n))


#Function for making data long. 
read_stacker <- function(folder){
  files <- list.files(path=path.expand(folder),full.names = T)
  ## Dropping for now
  #codes <- files[grepl('Codes_',files)]
  dates <- files[grepl('Dates',files)]
  q <- files[grepl('Values',files)]
  
  #make it long
  date_data <- read_csv(dates) %>%
    gather(key=id,value=date)
  q_data <- read_csv(q) %>%
    gather(key=id,value=q) %>%
    mutate(date=as.POSIXct(date_data$date,origin='1970-01-01')) %>%
    # changes substr for my filepath
    mutate(sat = substr(folder,33,41)) %>%
    separate(id,into=c("x", "id"), sep="X", remove=T, convert=T ) %>%
    mutate(id = as.character(id)) %>%
    dplyr::select(-x) 
    #mutate(id = gsub('X','0',id))  #%>%
 #   mutate(id=ifelse(substr(id,1,1) != 0 & nchar(id) < 8,paste0('0',id),id))
  return(q_data)
}

#Folders where data lives
folders <- c('D:/GoogleDrive/ROTFL/in/cleaned/Landsat_8/CloudsRemoved','D:/GoogleDrive/ROTFL/in/cleaned/Landsat_7/CloudsRemoved',
             'D:/GoogleDrive/ROTFL/in/cleaned/Landsat_5/CloudsRemoved')

folders_all <- c('D:/GoogleDrive/ROTFL/in/cleaned/Landsat_8/AllReturns','D:/GoogleDrive/ROTFL/in/cleaned/Landsat_7/AllReturns',
             'D:/GoogleDrive/ROTFL/in/cleaned/Landsat_5/AllReturns')

#Stack all landsats and remove unknown IDS. 
landsat_cloud_free <- map_dfr(folders,read_stacker) %>%
  filter(id != '01135') %>%
  filter(!is.na(date)) %>%
  filter(!is.na(q), q> 0) %>%
  mutate(id = str_pad(id, 8, pad="0", side="left"))

landsat_all <- map_dfr(folders_all,read_stacker) %>%
  filter(id != '01135') %>%
  filter(!is.na(date)) %>%
  filter(!is.na(q), q> 0) %>%
  mutate(id = str_pad(id, 8, pad="0", side="left"))

#Stack the usgs full dataset
usgs_full <- read_stacker(folder='D:/GoogleDrive/ROTFL/in/cleaned/Master_Tables')  %>%
  dplyr::select(-sat) %>%
  mutate(id = str_pad(id, 8, pad="0", side="left"))

#Make a separate dataset where landsat only counts if it is at the same 
#time q was taken
matched_sats <-  landsat_cloud_free %>%
  inner_join(usgs_full %>% dplyr::select(id,date),by=c('id','date'))

#Make long dataset of full usgs distribution with joined "landsat samples"
full_sats <- usgs_full %>% 
  rename(q_pop=q) %>%
  left_join(landsat_cloud_free %>% rename(q_sample=q), by=c('id','date'))

# Get the unique usgs ids
ids <- unique(usgs_full$id)

#Download site lat long and other info
sites <- readNWISsite(ids) %>% as_tibble() %>%
  rename(id=site_no)

#sites <- read_csv('D:/GoogleDrive/ROTFL/in/gauge_attributes/SiteAttributes.csv') %>%
#  rename(id=site_no) %>%
#  mutate(id = str_pad(id, 8, pad="0", side="left"),
#          id =as.character(id))

#Convert to spatial object
site_sf <- st_as_sf(sites,coords=c('dec_long_va','dec_lat_va'),crs=4326)
#Save as .RData file
save(landsat_cloud_free, landsat_all, matched_sats, usgs_full, site_sf,file='D:/GoogleDrive/ROTFL/out/rotfl_clean.RData')

#####################################################

#load('out/rotfl_clean.RData')

# 103 sites with all NA in discharge data. lets remove these
bad_sites <- usgs_full %>% 
  group_by(id) %>%
  mutate(n = sum(!is.na(q))) %>%
  filter(n ==0) %>%
  distinct(id, .keep_all = T)

# make nested data sets for mapping stats
# had to set filters to still remove sites with too little data
# that was messing up stats

# Cloud free landsat overpasses
nested_sat <- matched_sats %>%
  group_by(id) %>%
  mutate(n = sum(!is.na(q))) %>%
  # filter to sites that have > 10 Q samples over landsat record
  # cannot effectively compare distributions will small sample size
  dplyr::filter(n > 10) %>%
  dplyr::filter(!id %in% unique(bad_sites$id)) %>%
  nest() %>%
  rename(sat_data=data)

nested_gs <- usgs_full %>%
  group_by(id) %>%
  #mutate(n = sum(!is.na(q))) %>%
  dplyr::filter(id %in% unique(nested_sat$id)) %>%
  nest() %>%
  inner_join(nested_sat,by='id')

# All landsat overpasses
nested_all <- landsat_all %>%
  group_by(id) %>%
 # mutate(n = sum(!is.na(q))) %>%
  #dplyr::filter(n > 10) %>%
  dplyr::filter(id %in% unique(nested_sat$id)) %>%
  nest() %>%
  rename(sat_data=data)

nested_gs_all <- usgs_full %>%
  group_by(id) %>%
 # mutate(n = sum(!is.na(q))) %>%
  dplyr::filter(id %in% unique(nested_all$id)) %>%
  nest() %>%
  inner_join(nested_all,by='id')

## make map of fraction of days cloud free at sites
cloud <- landsat_all %>%
  dplyr::filter(id %in% unique(nested_sat$id)) %>%
  filter(!is.na(q)) %>%
  group_by(id) %>%
  summarise(count_all = n()) %>%
  left_join(landsat_cloud_free %>%
              dplyr::filter(id %in% unique(nested_sat$id)) %>%
              filter(!is.na(q)) %>%
              group_by(id) %>%
              summarise(count_cloudfree = n()),
            by="id"  ) %>%
  mutate(cloud_free_frac = count_cloudfree/count_all)
  
cloud_join <- cloud %>%
  left_join(sites, by="id") %>%
  st_as_sf(coords=c('dec_long_va','dec_lat_va'),crs=4326)

map_cloud<- mapview(cloud_join, zcol='cloud_free_frac', legend=T)

mapshot(map_cloud,  file="figs/cloud_map.png", url ="figs/cloud_map.html"  )

#write.csv(cloud_join,file='D:/GoogleDrive/ROTFL/out/cloudiness_gauges.csv')


### functions for mapping
myks <- function(full,sat){
  x = full %>%
    filter(!is.na(q),q > 0) %>%
    pull(q) %>%
    jitter()
  
  y = sat %>%
    filter(!is.na(q), q > 0) %>%
    pull(q) %>%
    jitter()
  
  tk <- ks.test(x,y)
  out = tibble(d=tk$statistic,ks_p.value=tk$p.value,nsat=length(y), npop=length(x),sdq=sd((x)),
               mq=mean((x)), mq_sat=mean((y)), sdq_sat=sd((y)))
  return(out)
}


myboots <- function(full,sat){
  x = full %>%
    filter(!is.na(q),q > 0) %>%
    pull(q)
  y = sat %>%
    filter(!is.na(q), q > 0) %>%
    pull(q)
  
  boot <- Matching::ks.boot(x,y,nboots=200)
  
  out = tibble(boot.d = boot$ks$statistic, boot_p.value = boot$ks.boot.pvalue  )
  
  return(out)
}

mywilcox <- function(full,sat){
  x = full %>%
    dplyr::filter(!is.na(q),q > 0) %>%
    pull(q) %>%
    jitter()
  
  y = sat %>%
    dplyr::filter(!is.na(q), q > 0) %>%
    pull(q) %>%
    jitter()
  
  tk <- wilcox.test(x,y, paired = F, correct=T)
  
  out = tibble(w=tk$statistic, wc_p.value=tk$p.value)
  
  return(out)
}

RBIcalc <- function(data){##FUNCTION.RBIcalc.START

  time.start <- proc.time();

  Q <- data %>% 
    pull(q) 
  
  Q[Q==0] <- NA
  # Size
  myLen <- length(Q)
  # Add previous record in second column
  Qprev <- c(NA,Q[-myLen])
  # Create dataframe.
  myData <- as.data.frame(cbind(Q,Qprev))
  # delta (absolute)
  myData[,"AbsDelta"] <- abs(myData[,"Q"] - myData[,"Qprev"])
  # SumQ
  SumQ <- sum(myData[,"Q"],na.rm=TRUE)
  # Sum Delta
  SumDelta <- sum(myData[,"AbsDelta"], na.rm=TRUE)
  
  RBIsum <- SumDelta / SumQ
  
  time.elaps <- proc.time()-time.start
  # Return RBI value 
  return(RBIsum)
}

mycvm <- function(full, sat) {
  require(twosamples)
  
  x = full %>%
  filter(!is.na(q),q > 0) %>%
  pull(q) %>%
  jitter()

y = sat %>%
  filter(!is.na(q), q > 0) %>%
  pull(q) %>%
  jitter()

cvm <- cvm_test(x,y, nboot=50)
out = tibble(stat = cvm[1], cvm_p.value=cvm[2])
return(out)
}

## TEST out stats on ONE site
# ID_test <-  "01010000"  # 01013500
# # test site for same 01010000
# full_sats %>%
#   #dplyr::filter(id %in% unique(id)[1:20]) %>%
#   dplyr::filter(id==ID_test) %>%
#   ggplot() +
#   geom_density(aes(q_pop), color="black") +
#   geom_density(aes(q_sample), color="red") +
#   #  scale_x_log10() +
#   scale_x_log10() +
#   theme_few() 
#  # facet_wrap(~id, scales="free")
#
# 
# test <- matched_sats %>% filter(id == ID_test, !is.na(q))
# test_all <- usgs_full %>% filter(id == ID_test, !is.na(q))
# #test_boot <- myboots(test_all, test)
# cvm <- cvm_test(test$q %>% jitter, test_all$q %>% jitter(), nboot=50)
# ks.test(test$q %>% jitter(), test_all$q %>% jitter())
# mycvm(test_all, test)



# Cloud free overpasses: do all stats test
w_test <- nested_gs %>% 
  #slice(1:100) %>%
  mutate(ks = map2(data,sat_data,myks),
         #wc = map2(data,sat_data,mywilcox),
         cvm = map2(data, sat_data, mycvm)) %>%
  mutate(rbi = map(data, RBIcalc)) %>%
 # unnest(wc) %>%
  unnest(ks) %>%
  unnest(rbi) %>%
  unnest(cvm) %>%
  dplyr::select(-data, -sat_data) %>%
  mutate(ks_p = ifelse(ks_p.value < 0.05, "different", "same"),
         #wc_p = ifelse(wc_p.value < 0.05, "different", "same"),
         cvm_p = ifelse(cvm_p.value < 0.05, "different", "same"))


# All landsat passes
w_test_all <- nested_gs_all %>% 
  #slice(1) %>%
  mutate(ks = map2(data,sat_data,myks),
         cvm = map2(data, sat_data, mycvm)) %>%
         #wc = map2(data,sat_data,mywilcox)) %>%
  mutate(rbi = map(data, RBIcalc)) %>%
  unnest(ks) %>%
  unnest(rbi) %>%
  unnest(cvm) %>%
  dplyr::select(-data, -sat_data) %>%
  mutate(ks_p = ifelse(ks_p.value < 0.05, "different", "same"),
         wc_p = ifelse(wc_p.value < 0.05, "different", "same"),
         cvm_p = ifelse(cvm_p.value < 0.05, "different", "same"))

# bootstrapped ks test on cloud free overpasses
boot_test <- nested_gs %>% 
  #slice(1) %>%
  mutate( boot = map2(data, sat_data, myboots)) %>%
  unnest(boot) %>%
  dplyr::select(-data, -sat_data) %>%
  mutate(boot_p = ifelse(boot_p.value < 0.05, "different", "same"))

###################################################

#take quick looks at the distributions of Q for a few sites.
# change the index numbers to pan around other sites
# full_sats %>%
#   #dplyr::filter(id %in% unique(id)[1:20]) %>%
#  dplyr::filter(id=="01199000") %>%
#   ggplot() +
#   geom_density(aes(q_pop), color="black") +
#   geom_density(aes(q_sample), color="red") +
# #  scale_x_log10() +
#   scale_x_log10() +
#   theme_few() +
#   facet_wrap(~id, scales="free")
# 
# # look at time series
# full_sats %>%
#   #dplyr::filter(id %in% unique(id)[1:4]) %>%
#    dplyr::filter(id=="06656000") %>%
#   ggplot() +
#   geom_line(aes(x=date, y=q_pop)) +
#   theme_few() +
#   facet_wrap(~id, scales="free")


# make look up table of population flow percentiles by site to later join
# to find what population percentiles corresponed with min/max sample Q
percentiles_pop <- full_sats %>%
  dplyr::select(id, q_pop) %>%
  arrange(id, q_pop) %>%
  group_by(id) %>%
  mutate(prob_pop = 1-cume_dist(q_pop)) %>%
  distinct(id, q_pop, prob_pop, .keep_all = T)
  

# Distributions look good. I think we can extract mode, max, min per gage.
# find mode, min, max for population and sample per site
join_sum <- full_sats %>%
  dplyr::select(id, q_sample, q_pop) %>%
  #arrange(id, q_pop) %>%
  # remove sites that have no landsat samples, or no gage flow data
  dplyr::filter(id %in% unique(nested_sat$id)) %>%
  # calculate modal, max, min Q for sample and population
  group_by(id, n_pop) %>%
  summarise_at(vars(q_sample, q_pop), funs(mode=modeest::mlv, min=min, max=max, med=median), na.rm=T) %>%
  mutate(mode_ratio = q_sample_mode / q_pop_mode) %>%
  ungroup() %>%
  # join in what "poplution" flow percentile corresponds to sample max/min 
  inner_join(., percentiles_pop, by=c("id", "q_sample_max"="q_pop")) %>%
  rename(prob_sample_max = prob_pop ) %>% 
  inner_join(., percentiles_pop, by=c("id", "q_sample_min"="q_pop")) %>%
  rename(prob_sample_min = prob_pop) %>% 
  mutate(percentile_range_sample = (prob_sample_min - prob_sample_max) *100)
  
# doesn't work well for arid rivers that do not have lognormal flow distribution
# these rivers have more of an exponential or some other EVD

join_sum_site <- join_sum %>%
  inner_join(sites, by = "id") 

# a bunch of sites get lost here, because they are missing in csv and/or when downloading NWIS site
w_test_join <- w_test %>%
  left_join(join_sum_site, by="id") %>%
  #dplyr::filter(!is.na(dec_long_va)) %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs= 4326) 


# make some maps of how well landsat captures modal and range of Q per gage 
#mapview(join_sum_site, zcol='percentile_range_sample', legend=T )

mapview(w_test_join, zcol='ks_p', legend=T )


w_test_all_join <- w_test_all %>%
  left_join(sites, by="id") %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs= 4326) 

mapview(w_test_all_join, zcol='d',  legend=T )

#####################################################
### other plots 

# is there a difference in flashiness based on if pop and sample distributions are 
# different?  NOPE
ggplot(w_test_join) +
  #geom_density(aes(rbi, color=wc_p)) +
 geom_point(aes(x=d, y=ks_p.value, color=ks_p)) +
  theme_few() 

# with drainage area ? NOPE
ggplot(w_test_join) +
  #geom_density(aes(rbi, color=ks_p)) +
  geom_point(aes(x=drain_area_va, y=d, color=ks_p)) +
  theme_few() +
  scale_x_log10()

#
ggplot(w_test_join) +
  geom_density(aes(drain_area_va, color=ks_p)) +
  #geom_point(aes(x=drain_area_va, y=d, color=ks_p)) +
  theme_few() +
  scale_x_log10()

ggplot(w_test_join) +
  geom_density(aes(sdq, color=ks_p)) +
  #geom_point(aes(x=drain_area_va, y=d, color=ks_p)) +
  theme_few() +
  scale_x_log10()


#
ggplot(w_test_join) +
  #geom_density(aes(rbi, color=ks_p)) +
  geom_point(aes(x=drain_area_va, y=d, color=ks_p.value)) +
  theme_few() +
  scale_x_log10()
#

ggplot(w_test) +
  geom_point(aes(x=npop, y =nsat, color=ks_p), size=2) +
  theme_few() +
  xlab("# of days in population Q at gage") +
  ylab("# of cloud free Landsat Q samples")

#ggsave(paste('figs/', "sample_size.jpeg", sep=""), units='in', width = 4, height=4,
#       dpi=350)

#
ggplot(w_test) +
  #geom_density(aes(rbi, color=ks_p)) +
  geom_point(aes(x=d, y=stat, color=cvm_p)) +
  theme_few() +
  xlab("D statistic (ks)") +
  ylab("CVM test statistic")
  #scale_x_log10()
ggsave(paste('figs/', "ks_cvm_stats.jpeg", sep=""), units='in', width = 4, height=4,
       dpi=350)

#

# look at pvlaues from wilcox and ks
ggplot(w_test) +
  geom_point(aes(x=cvm_p.value, y=ks_p.value, color=nsat)) +
  theme_few() +
  scale_x_log10() +
  scale_y_log10() +
  geom_hline(aes(yintercept = 0.05)) +
  geom_vline(aes(xintercept = 0.05)) +
  xlab("Wilcox p-value") +
  ylab("KS p-value")

ggsave(paste('figs/', "WC_KS_p.jpeg", sep=""), units='in', width = 4, height=4,
       dpi=350)

#
ggplot(w_test_join) +
  geom_point(aes(x=percentile_range_sample, y=d, color=nsat)) +
  theme_few() +
 # scale_x_log10() +
  scale_y_log10()  

  xlab("Wilcox p-value") +
  ylab("KS p-value")



# >90% of gages capture 97% perecntiles of flow
ggplot(join_sum_site) +
  geom_histogram(aes(percentile_range_sample)) +
  scale_y_log10() +
  xlab("Range of flow percentiles sampled by LS5,7,8") +
  ylab("# of gages") +
  theme_few()

#ggsave(paste('figs/', "range_captured.jpeg", sep=""), units='in', width = 5, height=5)

  

# most gages capture close to modal Q. Landsat slightly underestimates on average
ggplot(join_sum) +
  geom_density(aes(mode_ratio), fill="grey") +
  xlim(0, 5)+
  theme_bw() +
  geom_vline(xintercept=1, col="red") +
  xlab("Sample Modal Q / Pop Modal Q")

#ggsave(paste('figs/', "modalQ_hist.jpeg", sep=""), units='in', width = 6, height=4)

ggplot(join_sum_site) +
  geom_point(aes(q_pop_mode, q_sample_mode)) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  geom_abline(intercept=0, slope=1, col="red") +
  theme_few() +
  xlab("Modal Q (population)") +
  ylab("Modal Q (sample)")

#ggsave(paste('figs/', "modalQ_captured.jpeg", sep=""), units='in', width = 5, height=5)

# does the length of the Q record (population) impact how well? Not really
ggplot(join_sum_site) +
  geom_point(aes(n_pop, mode_ratio)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10() +

  geom_hline(yintercept=1, col="red")

# does watershed area affect how well landsat represents modal Q?
# Yes, there is a convergence to better representation as Area increases
ggplot(join_sum_site) +
  geom_point(aes(drain_area_va, mode_ratio)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ylim(0.2,5) +
  geom_hline(yintercept=1, col="red") +
  theme_few() +
  ylab("") +
  xlab("Drainage area (mi^2)") +
  ylab("Sample Modal Q / Pop Modal Q")

#ggsave(paste('figs/', "modalQ_drainage_area.jpeg", sep=""), units='in', width = 5, height=5)


###################################################

##################################################
### matt's plotting

myplotter <- function(full,sat){
  x = full %>%
    filter(!is.na(q),q > 0) %>%
    pull(q)
  y = sat %>%
    filter(!is.na(q), q > 0) %>%
    pull(q)
  xt = tibble(q=x,data='usgs')
  yt = tibble(q=y,data='sat')
  gp <- rbind(xt,yt) %>%
    ggplot(.,aes(x=q,color=data)) +
    geom_density(size=1) + 
    scale_color_manual(values=c('red3','black'),name='') + 
    scale_x_log10() + 
    theme_few() + 
    theme(legend.position = c(0.8,0.8)) + 
    ggtitle(paste('Cloud Free # =',length(y),sep=' '))
  return((gp))
}


nested_gs_mods <- nested_gs %>%
  mutate(ks = map2(data,sat_data,myks)) %>%
  mutate(gp = map2(data,sat_data,myplotter)) %>%
  unnest(ks) %>%
  mutate(d=round(d,4))


spatial <- site_sf %>%
  inner_join(nested_gs_mods,by='id')

qpal <- colorNumeric("Reds", spatial$d, n = 7)


## Map (takes a long time, don't run)
big_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addCircleMarkers(data=spatial,group='sat',color=~qpal(spatial$d)) %>%
  addLegend('bottomright',pal=qpal,values=spatial$d,
            title='D Value') %>%
  addPopupGraphs(spatial$gp,group='sat',width=250,height=250) 

mapshot(big_map,url='map.html')


d_plot <- nested_gs_mods %>%
  mutate(pcut = cut(p.value,breaks=c(0,0.001,0.01,0.05,0.1,1))) %>%
  filter(!is.na(pcut))

ggplot(d_plot,aes(x=nsat,y=d,color=pcut)) + 
  geom_point() + 
  theme_few() + 
  xlab('# of satellite images') + 
  ylab('D Value') + 
  theme(legend.position=c(0.7,0.7)) + 
  scale_x_log10()



#########################################################
### old munge

### load data
# wd <- "D:/GoogleDrive/ROTFL"
# 
# inDirPath = paste0(wd, '/in/cleaned')
# if (!file.exists(inDirPath)){dir.create(inDirPath)}
# CSVpaths_all = list.files(inDirPath, ".csv", recursive=T, full.names=T)
# 
# # write out PDFs in "out" directory:
# outDirPath = paste0(wd, "/out")
# if (!file.exists(outDirPath)){dir.create(outDirPath)}
# 
# # hard-coded names of CSV tables:
# mission = c("Landsat_5", "Landsat_7", "Landsat_8")
# master = c("Master")
# datatype = c("Date", "Value", "Code")
# dataset = c("CloudsRemoved", "AllReturns")
# 
# grepTerms = c(as.vector(outer(outer(mission, datatype, paste, sep=".*"), 
#                               dataset, paste, sep=".*")), 
#               paste(master, datatype, sep=".*"))
# tabNames = gsub("[.]", "_", gsub("[*]", "", grepTerms))
# 
# pathNamesList = lapply(grepTerms, grep, CSVpaths_all, value=T)
# 
# # read in csvs and assign them to variables based on file names:
# for (i in 1:length(tabNames)){
#   print(paste("Reading in", pathNamesList[[i]][1]))
#   assign(tabNames[i], read.csv(pathNamesList[[i]][1], header=T))
# }
# 
# site_info <- read_csv(paste(wd, "/SiteAttributes.csv", sep="")) %>%
#   mutate(site = as.character(site_no)) %>%
#   dplyr::select(-site_no)
# 
# 
# ### function to convert wide to long, and if bind =T, join site data
# wide2long <- function(date, value, code, site, bind=F) {
#   
#   date_long <- date %>%
#     gather(key = "site", value = "date") 
#   
#   value_long <- value %>%
#     gather(key = "site", value = "flow") %>%
#     dplyr::select(-site)
#   
#   code_long <- code %>%
#     gather(key="site", value = "code") %>%
#     dplyr::select(-site)
#   
#   if(bind ==T) {
#     out <- cbind(date_long, value_long) %>%
#       cbind(code_long) %>%
#       arrange(site, date) %>%
#       mutate(date = anydate(date)) %>%
#       separate(site,into=c("x", "site"), sep="X", remove=T, convert=T ) %>%
#       mutate(site = as.character(site)) %>%
#       dplyr::select(-x) %>%
#       left_join(site, b="site")
#     
#   } else{
#     out <- cbind(date_long, value_long) %>%
#       cbind(code_long) %>%
#       arrange(site, date) %>%
#       mutate(date = anydate(date)) %>%
#       separate(site,into=c("x", "site"), sep="X", remove=T, convert=T ) %>%
#       mutate(site = as.character(site)) %>%
#       dplyr::select(-x, -code)
#   }
#   
#   #rm(date, value, code)
#   return(out)
# }
# 
# # make master data long
# ms_data <- wide2long(date=Master_Date, value=Master_Value, code=Master_Code,
#                      site=site_info, bind=T)
# 
# # make landsat5 long for clouds removed data
# ls5 <- wide2long(date=Landsat_5_Date_CloudsRemoved, value=Landsat_5_Value_CloudsRemoved, 
#                  code=Landsat_5_Code_CloudsRemoved, bind=F) %>%
#   drop_na() %>%
#   mutate(sat = "ls_5") %>%
#   rename(flow_sample = flow)
# 
# # make landsat7 long
# ls7 <- wide2long(date=Landsat_7_Date_CloudsRemoved, value=Landsat_7_Value_CloudsRemoved, 
#                  code=Landsat_7_Code_CloudsRemoved, bind=F) %>%
#   drop_na()  %>%
#   mutate(sat = "ls_7") %>%
#   rename(flow_sample = flow)
# 
# # make landsat8 long
# ls8 <- wide2long(date=Landsat_8_Date_CloudsRemoved, value=Landsat_8_Value_CloudsRemoved, 
#                  code=Landsat_8_Code_CloudsRemoved, bind=F) %>%
#   drop_na() %>%
#   mutate(sat = "ls_8") %>%
#   rename(flow_sample = flow)
# 
# # row bind landsat missions
# # there are ~ 34000 duplicates where 5,7 ot 7,8 get sample from same site/date
# # I am guessing this is edge of scenes? Lets drop those duplicatesd
# # so we can do the stats for all landsat data combined. It doesn't matter which
# # mission sampled if its the same site/date, but IF want to do analysis per 
# # mission join the data individual and do NOT drop duplicates
# ls_all <- bind_rows(ls5, ls7, ls8) %>%
#   distinct(site, date, flow_sample, .keep_all = T)
# 
# # dups <- ls_all %>%
# #   drop_na() %>%
# #   group_by(site, date,flow_sample) %>%
# #   mutate(n = n()) %>%
# #   filter(n >1) %>%
# #   arrange(site, date, flow_sample)
# 
# # write data
# #write_feather(ms_data, "out/master_data_long.feather")
# 
# #write_feather(ls_all, "out/ls_data_long.feather")
# 
# ######################################################
# ms_data <- read_feather("out/master_data_long.feather")
# 
# ls_all <- read_feather("out/ls_data_long.feather")
# 
# 
# ### join landsat "sampled" flow to master "population" flow
# ms_join <- ms_data %>%
#   rename(flow_pop = flow) %>%
#   left_join(ls_all, by=c("site", "date")) 

#############################################################
#### Test out different statistics for assessind difference in distribution

## pick to example sites
# site that has the same distribution
# good <- ms_join %>% filter(site == "6038800")
# 
# # site that should have different distribution
# bad <- ms_join %>% filter(site == "14312000")
# #3314500, #3287000, 1351500, 14312000, 
# 
# good_long <- good %>%
#   dplyr::select(flow_pop, flow_sample) %>%
#   gather( key = "group", value = "flow") %>%
#   drop_na() %>%
#   mutate(group = as.factor(group)) %>%
#   as_tibble()
# 
# bad_long <- bad %>%
#   dplyr::select(flow_pop, flow_sample) %>%
#   gather( key = "group", value = "flow") %>%
#   drop_na() %>%
#   mutate(group = as.factor(group)) %>%
#   as_tibble()
# 
# 
# ggplot(good_long)+
#   geom_histogram(aes(flow, fill = group)) +
#   scale_x_log10() +
#   scale_y_log10() 
# 
# ggplot(bad_long)+
#   geom_histogram(aes(flow, fill = group)) +
#   scale_x_log10() +
#   scale_y_log10() 
# 
# 
# 
# ## ks test
# # says distributions are not different p > 0.05, D = 0.03
# ks.test(good$flow_pop, good$flow_sample)
# 
# # says distributions are different p < 0.05, D = .26
# ks.test(bad$flow_pop, bad$flow_sample)
# 
# 
# ## equivalence test , this test says both good/bad are the same distribution
# tost(log(good$flow_pop), log(good$flow_sample), paired = F, var.equal = F, conf.level = 0.95)
# 
# tost(log(bad$flow_pop), log(bad$flow_sample), paired = F, var.equal = F, conf.level = 0.95)
# 
# 
# ## try shift test that looks for difference between any quantile.
# ## this test takes about 3 minutes
# shift_good <- rogme::shifthd_pbci(data=good_long, flow ~ group, q=c(0.25, 0.5, 0.75)) 
# 
# shift_bad <- rogme::shifthd_pbci(data=bad_long, flow ~ group, q=c(0.25, 0.5, 0.75)) 
# 
# # all qunatiles have P > 0.05 and distributions are the same
# shift_good
# # all qunatiles have P < 0.05 and disttributions are different
# shift_bad
# 
# rogme::plot_sf(shift_good, plot_theme = 2)[[1]] 
# 
# rogme::plot_sf(shift_bad, plot_theme = 2)[[1]] 

